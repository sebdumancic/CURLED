package representationLearning.layer

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.AbstractBagComparison
import relationalClustering.bagComparison.bagCombination.AbstractBagCombine
import relationalClustering.clustering.algo.AbstractSKLearnCluster
import relationalClustering.clustering.evaluation.AbstractEvaluatorModel
import relationalClustering.representation.domain.KnowledgeBase
import representationLearning.clusterComparison.AbstractClusterOverlap
import representationLearning.clusterSelection.ModelBasedSelection

/**
  * Created by seb on 02.03.16.
  */
class MaxLayer(override protected val rootFolder: String,
               override protected val outputName: String,
               override protected val knowledgeBase: KnowledgeBase,
               override protected val domainsToCluster: List[String],
               override protected val depth: Int,
               override protected val bagCompare: AbstractBagComparison,
               override protected val bagCombination: AbstractBagCombine,
               override protected val aggregators: List[AbstractAggregator],
               override protected val preserveVertexOrder: Boolean,
               override protected val vertexCombination: String,
               override protected val clusteringAlg: AbstractSKLearnCluster,
               protected val clusterEval: AbstractEvaluatorModel,
               override protected val clusterOverlap: AbstractClusterOverlap,
               override protected val overlapThreshold: Double,
               override protected val maxClusters: Int,
               override protected val parameterList: List[List[Double]],
               override protected val doClusterHyperedges: Boolean,
               override protected val asFeature: Boolean) extends AdaptiveSelectionLayer(rootFolder, outputName, knowledgeBase, domainsToCluster, depth, bagCompare, bagCombination,
                                                                                          aggregators, preserveVertexOrder, vertexCombination, clusteringAlg, new ModelBasedSelection(clusterEval), clusterOverlap,
                                                                                                 overlapThreshold, maxClusters, parameterList, doClusterHyperedges, asFeature) {

}
