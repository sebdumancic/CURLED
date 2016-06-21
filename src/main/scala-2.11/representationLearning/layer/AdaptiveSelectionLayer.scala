package representationLearning.layer

import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.clustering.AbstractSKLearnCluster
import relationalClustering.representation.clustering.Clustering
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.{SimilarityNTv2, SimilarityNeighbourhoodTrees}
import relationalClustering.utils.Settings
import representationLearning.clusterComparison.AbstractClusterOverlap
import representationLearning.clusterSelection.AbstractClusterSelection
import representationLearning.representation.ClusteringRepresentation


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
                             override protected val maxClusters: Int,
                             protected val parameterList: List[List[Double]],
                             protected val doClusterHyperedges: Boolean,
                             override protected val asFeature: Boolean) extends AbstractLayer(rootFolder, outputName, maxClusters, asFeature) {

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
    val allCreatedClusters = collection.mutable.Set[Clustering]()

    parameterList.zipWithIndex.foreach(pars => {
      println(s"---- using parameters ${pars._1}")

      val similarityMeasure = measureIdentifier match {
        case "RCNT" => new SimilarityNeighbourhoodTrees(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
        case "RCNTv2" => new SimilarityNTv2(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
      }

      var createdClusters = List[Clustering]()

      //val filename = similarityMeasure.getObjectSimilaritySave(List(dom), getRoot)

      (2 to maxClusters).foreach(numCl => {
        createdClusters = createdClusters :+ clusteringAlg.clusterVertices(List(dom), similarityMeasure, numCl, pars._2) // clusteringAlg.clusterFromFile(filename._1, math.min(numCl, filename._2.length - 1))
      })

      val selectedCluster = clusterSelect.selectFromClusters(createdClusters)

      allCreatedClusters.nonEmpty && selectedCluster.getClusters.length > 1 match {
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
          if (selectedCluster.getClusters.length > 1) {
            allCreatedClusters += selectedCluster
          }
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
    val allCreatedClusters = collection.mutable.Set[Clustering]()

    parameterList.zipWithIndex.foreach(pars => {
      println(s"---- using parameters ${pars._1}")

      val similarityMeasure = measureIdentifier match {
        case "RCNT" => new SimilarityNeighbourhoodTrees(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
        case "RCNTv2" => new SimilarityNTv2(knowledgeBase, depth, pars._1, bagCompare, bagCombination, false)
      }

      var createdClusters = List[Clustering]()
      //val filename = similarityMeasure.getHyperEdgeSimilaritySave(doms, getRoot)

      (2 to maxClusters).foreach(numCl => {
        createdClusters = createdClusters :+ clusteringAlg.clusterEdges(doms, similarityMeasure, numCl, pars._2) //clusteringAlg.clusterFromFile(filename._1, math.min(numCl, filename._2.length - 1))
      })

      val selectedCluster = clusterSelect.selectFromClusters(createdClusters)

      allCreatedClusters.nonEmpty match {
        case true =>
          val maxOverlap = allCreatedClusters.map(cl => clusterOverlap.compare(cl, selectedCluster)).max
          if (maxOverlap < overlapThreshold) {
            allCreatedClusters += selectedCluster
            println(s"---- ---- ---- Cluster accepted ($maxOverlap)")
          }
          else {
            println(s"---- ---- ---- Cluster rejected because $maxOverlap: ${pars._1}, $doms")
          }
        case false =>
          allCreatedClusters += selectedCluster
      }

      similarityMeasure.clearCache()
    })

    allCreatedClusters.toSet
  }

  /** Build a layer by clustering each specified domain and the existing hyperedges is specified
    *
    * @return new representation obtained with clustering
    **/
  def build() = {

    var clusters = domainsToCluster.foldLeft(Set[Clustering]())( (acc, dom) => {
      acc ++ clusterDomain(dom)
    })

    if (doClusterHyperedges) {
      val hyperedgeDomains = knowledgeBase.getExistingHyperedges(2, domainsToCluster)
      clusters = clusters ++ hyperedgeDomains.foldLeft(Set[Clustering]())( (acc, comb) => {
        acc ++ clusterHyperedges(comb)
      })
    }

    closeFiles()
    new ClusteringRepresentation(clusters)
  }
}
