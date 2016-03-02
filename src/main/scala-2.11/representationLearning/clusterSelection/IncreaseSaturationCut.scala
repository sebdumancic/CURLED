package representationLearning.clusterSelection

import relationalClustering.clustering.evaluation.AbstractEvaluatorModel

/** Selects the first cluster for which the following equation holds:
  * sim(C(i)) >= factor * sim(C(i+1))
  *
  * assumes that clusters are listed in the increasing order and that the score increases with the number of clusters.
  * If none of the clusters satisfies the equation, returns the clustering with the highest score
  * *
  * Created by seb on 19.02.16.
  */
class IncreaseSaturationCut(protected val evaluateSingle: AbstractEvaluatorModel,
                            protected val factor: Double) extends AbstractClusterSelection {

  /** Applies the evaluation procedure
    *
    * @param clusterSet               a set of clusterings
    * @param elementOrdering          an ordering of the elements in the similarity matrix
    * @param similarityMatrixFileName the name of the file containing the similarity matrix
    * */
  override def selectFromClusters(clusterSet: List[Set[List[String]]], elementOrdering: List[String], similarityMatrixFileName: String) = {

    val evals = clusterSet.map(cluster => evaluateSingle.validate(cluster, elementOrdering, similarityMatrixFileName)).zipWithIndex
    val cands = evals.dropRight(1).map( item => item._1 >= (factor * evals(item._2 + 1)._1)).indexOf(true)
    println(s"---- ---- saturation selection::measures for clusters: ${evals.map(x => s"${x._1}:::${x._2 + 2}")}")

    cands == -1 match {
      case false => clusterSet(cands)
      case true => clusterSet.maxBy(cluster => evaluateSingle.validate(cluster, elementOrdering, similarityMatrixFileName))
    }
  }

}
