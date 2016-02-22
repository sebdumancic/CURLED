package representationLearning.clusterSelection

import relationalClustering.clustering.evaluation.AbstractEvaluatorModel

/**
  * Created by seb on 19.02.16.
  */
class IncreaseSaturationCut(override protected val similarityMatrixFileName: String,
                            override protected val elementOrdering: List[String],
                            override protected val evaluateSingle: AbstractEvaluatorModel,
                            protected val factor: Double) extends ModelBasedSelection(similarityMatrixFileName, elementOrdering, evaluateSingle) {

  /** Selects the first cluster for which the following equation holds:
    * sim(C(i)) >= factor * sim(C(i+1))
    *
    * assumes that clusters are listed in the increasing order and that the score increases with the number of clusters.
    * If none of the clusters satisfies the equation, returns the clustering with the highest score
    * */
  override def selectFromClusters(clusterSet: List[Set[List[String]]]) = {
    val evals = clusterSet.map( cluster => evaluateSingle.validate(cluster, getElementOrder, getSimilarityMatrixFile)).zipWithIndex
    val cands = evals.dropRight(1).map( item => item._1 >= (factor * evals(item._2 + 1)._1)).indexOf(true)
    println(s"---- ---- saturation selection::measures for clusters: ${evals.map(x => s"${x._1}:::${x._2 + 2}")}")

    cands == -1 match {
      case false => clusterSet(cands)
      case true => clusterSet.maxBy(cluster => evaluateSingle.validate(cluster, getElementOrder, getSimilarityMatrixFile))
    }
  }

}
