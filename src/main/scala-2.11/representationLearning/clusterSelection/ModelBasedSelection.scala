package representationLearning.clusterSelection

import relationalClustering.clustering.evaluation.AbstractEvaluatorModel

/**
  * Created by seb on 10.02.16.
  */
class ModelBasedSelection(protected val similarityMatrixFileName: String,
                          protected val elementOrdering: List[String],
                          protected val evaluateSingle: AbstractEvaluatorModel) extends AbstractClusterSelection {

  /** Returns the file containing the similarity matrix */
  def getSimilarityMatrixFile = {
    similarityMatrixFileName
  }

  /** Returns the element ordering */
  def getElementOrder = {
    elementOrdering
  }

  def selectFromClusters(clusterSet: List[Set[List[String]]]) = {
    clusterSet.map( cluster => new Tuple2(cluster, evaluateSingle.validate(cluster, getElementOrder, getSimilarityMatrixFile))).maxBy(_._2)._1
  }
}
