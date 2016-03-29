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

    /*val evals = clusterSet.map(cluster => evaluateSingle.validate(cluster, elementOrdering, similarityMatrixFileName)).zipWithIndex
    val cands = evals.dropRight(1).map( item => item._1 >= (factor * evals(item._2 + 1)._1)).indexOf(true)
    println(s"---- ---- saturation selection::measures for clusters: ${evals.map(x => s"${x._1}:::${x._2 + 2}")}")*/

    val evaluated = clusterSet.map( cl => (cl, evaluateSingle.validate(cl, elementOrdering, similarityMatrixFileName))).sortBy(_._2)
    println(s"---- evaluated clusters: ${evaluated.map( cl => (cl._1.size, cl._2))}")
    val newFactors = evaluated.map(_._2).zipWithIndex.map(cl => {
      val previousCl = (cl._2 - 1) < 0 match {
        case true => evaluateSingle.validate(Set(elementOrdering), elementOrdering, similarityMatrixFileName)
        case false => evaluated(cl._2 - 1)._2
      }

      val consecutiveCl = (cl._2 + 1) >= evaluated.length match {
        case true => 0
        case false => evaluated(cl._2 + 1)._2
      }

      math.abs( (previousCl - evaluated(cl._2)._2)/(evaluated(cl._2)._2 - consecutiveCl)) - (factor * evaluated(cl._2)._1.size)
    })
    println(s"---- new factors: $newFactors")

    //select the one with the highest score
    val cand = newFactors.zipWithIndex.maxBy(_._1)._2
    println(s"---- selected clustering with index $cand")
    evaluated(cand)._1
  }

}
