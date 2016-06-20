package representationLearning.layer

import java.io.{BufferedWriter, FileWriter}

import learners.ilp.ace.{TildeInduce, TildeNFold}
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.clustering.AbstractSKLearnCluster
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.{SimilarityNTv2, SimilarityNeighbourhoodTrees}
import relationalClustering.utils.{PredicateDeclarations, Settings}
import representationLearning.clusterComparison.AbstractClusterOverlap

/** Creates a new layer by increasing the number of clusters until general definition of the clusters can be found (coverage > minimalCoverage), or maximal number of
  *   clusters is exceeded
  * Created by seb on 22.03.16.
  */
class DefinitionBasedLayer(protected val knowledgeBase: KnowledgeBase,
                           protected val domainsToCluster: List[String],
                           protected val depth: Int,
                           override protected val maxClusters: Int,
                           protected val measureIdentifier: String,
                           protected val bagCompare: AbstractBagComparison,
                           protected val bagCombination: AbstractBagCombine,
                           protected val clusteringAlg: AbstractSKLearnCluster,
                           protected val minimalCoverage: Double,
                           protected val definitionLearner: Map[String,String],
                           protected val parameterList: List[List[Double]],
                           protected val clusterOverlap: AbstractClusterOverlap,
                           protected val overlapThreshold: Double,
                           protected val doClusterHyperedges: Boolean,
                           override protected val outputName: String,
                           override protected val rootFolder: String,
                           override protected val asFeature: Boolean) extends AbstractLayer(rootFolder, outputName, maxClusters, asFeature) {

  /** Returns the maximal number of clusters */
  def getMaxClusters = {
    maxClusters
  }

  /** Returns the knowledge base */
  def getKB = {
    knowledgeBase
  }

  /** Prepared temporary database, declarations and header of the new predicate candidates
    *
    * @param clustering clustering
    * @param doms domains of interest
    * */
  protected def prepareTemporaryDB(clustering: Set[List[String]], doms: List[String]) = {
    val writerDB = new BufferedWriter(new FileWriter(s"$getRoot/tmp.db"))
    val writerDecl = new BufferedWriter(new FileWriter(s"$getRoot/tmp.decl"))
    var header = ""

    clustering.zipWithIndex.foreach( clust => {
      writerDecl.write(s"Cluster${clust._2}(${doms.map( x => "name").mkString(",")})${sys.props("line.separator")}")
      header = header + s"Cluster${clust._2}(${doms.mkString(",")})\n"
      writerDB.write(s"${clust._1.map( el => s"Cluster${clust._2}(${el.replace(":",",")})").mkString(s"${sys.props("line.separator")}")}")
    })

    writerDB.close()
    writerDecl.close()

    (s"$getRoot/tmp.db", header, s"$getRoot/tmp.decl")
  }

  /** Evaluates a given clustering on the purity of definitions of the clusters
    *
    * @param clustering a clsutering of interest
    * @param dom domain(s) of cluster elements
    * @return total number of small-coverage rules in clustering
    * */
  protected def evaluateClustering(clustering: Set[List[String]], dom: List[String]) = {
    val files = prepareTemporaryDB(clustering, dom)
    val tmpDeclarations = new PredicateDeclarations(files._3)
    val tmpKB = new KnowledgeBase(List(files._1), files._2, tmpDeclarations)

    val evals = clustering.zipWithIndex.map( cl => (cl._1, evaluateCluster(cl._1, s"Cluster${cl._2}", dom, tmpKB)))

    evals.map( _._2).sum
  }

  /** Evaluates a single cluster, based on the number of small-coverage definitions (number of examples covered by the rules is less than 'minimalCoverage')
    *
    * @param cluster a cluster of interest; list of elements
    * @param targetPredicate target predicate name (name of the cluster in temporary knowledge base)
    * @param dom domains of the cluster
    * @param tmpKB latent knowledge base
    * @return a number of small-coverage rules
    * */
  protected def evaluateCluster(cluster: List[String], targetPredicate: String, dom: List[String], tmpKB: KnowledgeBase) = {
    val defLearner = definitionLearner("algorithm") match {
      case "TildeInduce" => new TildeInduce(getRoot, getKB, tmpKB, targetPredicate, if (dom.length > 1) true else false, definitionLearner("ACE_ROOT"),
        definitionLearner("heuristic"), definitionLearner("minCases").toInt)
      case "TildeNFold" => new TildeNFold(getRoot, getKB, tmpKB, targetPredicate, if (dom.length > 1) true else false, definitionLearner("ACE_ROOT"),
        definitionLearner("numFolds").toInt, definitionLearner("heuristic"), definitionLearner("minCases").toInt)
    }

    defLearner.fitModel()
    val rules = defLearner.getDefinitions

    println(s"---- ---- cluster coverages (max instances: ${cluster.length}): ${rules.toList.map(r => r.getAbsCoverage)}")

    rules.nonEmpty match {
      case false => 10 //if there are no rules, add a fixed penalty
      case true => rules.count( rule => rule.getAbsCoverage < (minimalCoverage * cluster.length))
    }
  }

  /** Finds the 'optimal' number of clusters of the specified domain, given the parameters
    *
    * @param dom domain of interest
    * @param pars parameters for the similarity measure
    * */
  protected def clusterDomainWithParameters(dom: String, pars: List[Double]) = {
    println(s"---- using parameters $pars")

    val similarityMeasure = measureIdentifier match {
      case "RCNT" => new SimilarityNeighbourhoodTrees(knowledgeBase, depth, pars, bagCompare, bagCombination, false)
      case "RCNTv2" => new SimilarityNTv2(knowledgeBase, depth, pars, bagCompare, bagCombination, false)
    }

    val filename = similarityMeasure.getObjectSimilaritySave(List(dom), getRoot)

    val clusterEvals = (2 to getMaxClusters).map( numClust => {
      val currentClustering = clusteringAlg.clusterFromFile(filename._1, math.min(numClust, filename._2.length - 1))
      (currentClustering, evaluateClustering(currentClustering, List(dom)))
    })

    println(s"---- clustering evaluations: ${clusterEvals.map( cl => (cl._1.size, cl._2))}")

    clusterEvals.map( _._2).contains(0) match {
      case true => clusterEvals(clusterEvals.map(_._2).indexOf(0))._1
      case false => clusterEvals.minBy(_._2)._1
    }
  }

  /** Clusters the specified domain
    *
    * @param dom domain to cluster
    * @return a set of all selected clusters
    **/
  protected def clusterDomain(dom: String) = {
    println(s"Clustering domain $dom")
    val allCreatedClusters = collection.mutable.Set[Set[List[String]]]()

    parameterList.foreach(par => {
      val selectedCluster = clusterDomainWithParameters(dom, par)

      allCreatedClusters.nonEmpty && selectedCluster.size > 1 match {
        case true =>
          val maxOverlap = allCreatedClusters.map(cl => clusterOverlap.compare(cl, selectedCluster)).max
          if (maxOverlap < overlapThreshold) {
            allCreatedClusters += selectedCluster
            println(s"---- ---- ---- Cluster accepted ($maxOverlap)")
          }
          else {
            println(s"---- ---- ---- Cluster rejected because $maxOverlap: $par, $dom")
          }
        case false =>
          if (selectedCluster.size > 1) {
            allCreatedClusters += selectedCluster
          }
      }
    })

    allCreatedClusters.toSet
  }

  /** Selects the optimal number of clusters for hyperedges given the parameters
    *
    * @param domains hyperedge domains
    * @param pars parameters for similarity measure
    * @return selected clustering
    * */
  protected def clusterHyperedgesWithParameters(domains: List[String], pars: List[Double]) = {
    println(s"---- using parameters $pars")

    val similarityMeasure = measureIdentifier match {
      case "RCNT" => new SimilarityNeighbourhoodTrees(knowledgeBase, depth, pars, bagCompare, bagCombination, false)
      case "RCNTv2" => new SimilarityNTv2(knowledgeBase, depth, pars, bagCompare, bagCombination, false)
    }

    val filename = similarityMeasure.getHyperEdgeSimilaritySave(domains, getRoot)

    val clusterEvals = (2 to getMaxClusters).map( numClust => {
      val currentClustering = clusteringAlg.clusterFromFile(filename._1, math.min(numClust, filename._2.length - 1))
      (currentClustering, evaluateClustering(currentClustering, domains))
    })

    clusterEvals.map(_._2).contains(0) match {
      case true => clusterEvals(clusterEvals.map(_._2).indexOf(0))._1
      case false => clusterEvals.minBy(_._2)._1
    }
  }

  /** Clusters the specified domain over all parameters
    *
    * @param domains domains of hyperedges
    * @return a set of clusterings
    * */
  protected def clusterHyperedges(domains: List[String]) = {
    println(s"Clustering domains $domains")
    val allCreatedClusters = collection.mutable.Set[Set[List[String]]]()

    parameterList.foreach( pars => {
      val selectedCluster = clusterHyperedgesWithParameters(domains, pars)

      allCreatedClusters.nonEmpty && selectedCluster.size > 1 match {
        case true =>
          val maxOverlap = allCreatedClusters.map(cl => clusterOverlap.compare(cl, selectedCluster)).max
          if (maxOverlap < overlapThreshold) {
            allCreatedClusters += selectedCluster
            println(s"---- ---- ---- Cluster accepted ($maxOverlap)")
          }
          else {
            println(s"---- ---- ---- Cluster rejected because $maxOverlap: $pars, $domains")
          }
        case false =>
          if (selectedCluster.size > 1) {
            allCreatedClusters += selectedCluster
          }
      }
    })

    allCreatedClusters.toSet
  }

  /** Checks whether a hyperEdge between specified domains exists in a knowledge base
    *
    * @param doms list of domains in a hyperEdge
    * @return [[Boolean]]
    **/
  def existsConnection(doms: List[String]) = {
    knowledgeBase.getPredicateNames.map(knowledgeBase.getPredicate).filter(_.getRole == Settings.ROLE_HYPEREDGE).foldLeft(false)((acc, pred) => {
      acc || pred.getDomains.sorted.combinations(doms.length).map(_.toList).contains(doms)
    })
  }

  /** Build a layer by clustering each specified domain and the existing hyperedges is specified
    *
    * @return (predicate definitions file, predicate declarations file, knowledge base file) - all file paths
    **/
  def build() = {

    domainsToCluster.foreach(dom => {
      val res = clusterDomain(dom)
      writeFiles(res, List(dom))
    })

    if (doClusterHyperedges) {
      (domainsToCluster ++ domainsToCluster).sorted.combinations(2).filter(com => existsConnection(com)).foreach(comb => {
        val res = clusterHyperedges(comb)
        writeFiles(res, comb)
      })
    }

    closeFiles()
    (getHeaderName, getDeclarationsName, getKBName)
  }

}
