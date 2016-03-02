package representationLearning.layer

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.clustering.AbstractSKLearnCluster
import relationalClustering.representation.KnowledgeBase
import relationalClustering.similarity.{SimilarityNTv2, SimilarityNeighbourhoodTrees}
import relationalClustering.utils.Settings
import representationLearning.clusterComparison.AbstractClusterOverlap
import representationLearning.clusterSelection.AbstractClusterSelection


/**
  * Created by seb on 29.02.16.
  */
class AdaptiveSelectionLayer(override protected val rootFolder: String,
                             override protected val outputName: String,
                             protected val knowledgeBase: KnowledgeBase,
                             protected val domainsToCluster: List[String],
                             protected val depth: Int,
                             protected val bagCompare: AbstractBagComparison,
                             protected val bagCombination: AbstractBagCombine,
                             protected val measureIdentifier: String,
                             protected val clusteringAlg: AbstractSKLearnCluster,
                             protected val clusterSelect: AbstractClusterSelection,
                             protected val clusterOverlap: AbstractClusterOverlap,
                             protected val overlapThreshold: Double,
                             protected val maxClusters: Int,
                             protected val parameterList: List[List[Double]],
                             protected val clusterHyperedges: Boolean) extends AbstractLayer(rootFolder, outputName) {

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

  /** Clusters the specified domain
    *
    * @param dom domain to cluster
    * @return a set of all selected clusters
    **/
  protected def clusterDomain(dom: String) = {
    println(s"Clustering domain: $dom")
    val allCreatedClusters = collection.mutable.Set[Set[List[String]]]()

    parameterList.zipWithIndex.foreach(pars => {
      println(s"---- using parameters ${pars._1}")

      val similarityMeasure = measureIdentifier match {
        case "RCNT" => new SimilarityNeighbourhoodTrees(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
        case "RCNTv2" => new SimilarityNTv2(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
      }

      var createdClusters = List[Set[List[String]]]()

      val filename = similarityMeasure.getObjectSimilaritySave(List(dom), getRoot)

      (2 until maxClusters).foreach(numCl => {
        createdClusters = createdClusters :+ clusteringAlg.clusterFromFile(filename._1, math.min(numCl, filename._2.length - 1))
      })

      val selectedCluster = clusterSelect.selectFromClusters(createdClusters, filename._2.map(_._1), filename._1)

      allCreatedClusters.nonEmpty match {
        case true =>
          val maxOverlap = allCreatedClusters.map(cl => clusterOverlap.compare(cl, selectedCluster)).max
          if (maxOverlap < overlapThreshold) {
            allCreatedClusters += selectedCluster
            println(s"---- ---- ---- Cluster accepted ($maxOverlap)")
          }
          else {
            println(s"---- ---- ---- Cluster rejected because $maxOverlap: $pars, $dom")
          }
        case false =>
          allCreatedClusters += selectedCluster
      }

      similarityMeasure.clearCache()
    })

    allCreatedClusters.toSet
  }

  /** Clusters hyperedge with the specified domains
    *
    * @param doms domains of the hyperedges
    * @return a set of clusterings (a set of lists)
    **/
  protected def clusterHyperedges(doms: List[String]) = {
    println(s"Clustering hyperedges: $doms")
    val allCreatedClusters = collection.mutable.Set[Set[List[String]]]()

    parameterList.zipWithIndex.foreach(pars => {
      println(s"---- using parameters ${pars._1}")

      val similarityMeasure = measureIdentifier match {
        case "RCNT" => new SimilarityNeighbourhoodTrees(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
        case "RCNTv2" => new SimilarityNTv2(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
      }

      var createdClusters = List[Set[List[String]]]()
      val filename = similarityMeasure.getHyperEdgeSimilaritySave(doms, getRoot)

      (2 until maxClusters).foreach(numCl => {
        createdClusters = createdClusters :+ clusteringAlg.clusterFromFile(filename._1, math.min(numCl, filename._2.length - 1))
      })

      val selectedCluster = clusterSelect.selectFromClusters(createdClusters, filename._2.map(_.mkString(":")), filename._1)

      allCreatedClusters.nonEmpty match {
        case true =>
          val maxOverlap = allCreatedClusters.map(cl => clusterOverlap.compare(cl, selectedCluster)).max
          if (maxOverlap < overlapThreshold) {
            allCreatedClusters += selectedCluster
            println(s"---- ---- ---- Cluster accepted ($maxOverlap)")
          }
          else {
            println(s"---- ---- ---- Cluster rejected because $maxOverlap: $pars, $doms")
          }
        case false =>
          allCreatedClusters += selectedCluster
      }

      similarityMeasure.clearCache()
    })

    allCreatedClusters.toSet
  }

  /** Writes the provided clusterings in a file
    *
    * @param clusters a set of clusterings (set of lists)
    * @param domains  domains of the clusterings
    **/
  protected def writeFiles(clusters: Set[Set[List[String]]], domains: List[String]) = {
    clusters.zipWithIndex.foreach(clustering => {
      clustering._1.zipWithIndex.foreach(clust => {
        getHeaderFile.write(s"Cluster_${domains.mkString("_")}${clust._2 + (clustering._2 * maxClusters)}(${domains.mkString(",")})\n")
        getDeclFile.write(s"Cluster_${domains.mkString("_")}${clust._2 + (clustering._2 * maxClusters)}(${domains.map(x => "name").mkString(",")})\n")
        getKBFile.write(clust._1.map(elem => s"Cluster_${domains.mkString("_")}${clust._2 + (clustering._2 * maxClusters)}(${elem.replace(":", ",")})").mkString("\n") + "\n")
      })

      getHeaderFile.write("\n")
      getDeclFile.write("\n")
      getKBFile.write("\n")
    })

    getHeaderFile.flush()
    getDeclFile.flush()
    getKBFile.flush()
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

    if (clusterHyperedges) {
      (domainsToCluster ++ domainsToCluster).sorted.combinations(2).filter(com => existsConnection(com)).foreach(comb => {
        val res = clusterHyperedges(comb)
        writeFiles(res, comb)
      })
    }

    closeFiles()
    (getHeaderName, getDeclarationsName, getKBName)
  }
}