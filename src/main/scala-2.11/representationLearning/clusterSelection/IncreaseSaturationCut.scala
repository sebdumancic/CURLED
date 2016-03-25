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
    val tmpSat = evaluated.map(_._2).zipWithIndex.dropRight(1).map(item => item._1 >= (factor * evaluated(item._2 + 1)._2))
    val relImprov = evaluated.map(_._2).zipWithIndex.dropRight(1).map( item => evaluated(item._2 +1)._2/item._1)
    val improvSaturation = relImprov.dropRight(1).zipWithIndex.map( item => (factor * item._1) <= relImprov(item._2 + 1))
    val saturated = improvSaturation.indexOf(true) < improvSaturation.indexOf(false) match {
      case false => improvSaturation.indexOf(true)
      case true => improvSaturation.indexOf(true, math.max(improvSaturation.indexOf(false), 0))
    }
    println(s"---- ---- cluster quality: ${evaluated.map(cl => (cl._1.size, cl._2))}\n" +
            s"---- ---- relative improvement: ${relImprov.zip(improvSaturation)} \n" +
            s"---- ---- selected $saturated ")

    /*cands == -1 match {
      case false => clusterSet(cands)
      case true => clusterSet.maxBy(cluster => evaluateSingle.validate(cluster, elementOrdering, similarityMatrixFileName))
    }*/

    saturated == -1 match {
      case false => evaluated(saturated)._1
      case true => evaluated.maxBy(_._2)._1
    }
  }

}
