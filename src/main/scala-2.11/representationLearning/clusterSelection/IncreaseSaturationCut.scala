package representationLearning.clusterSelection

import relationalClustering.clustering.evaluation.unsupervised.AbstractEvaluatorModel
import relationalClustering.representation.clustering.{Cluster, Clustering}


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
    * @param clusterSet a set of clusterings
    * */
  override def selectFromClusters(clusterSet: List[Clustering]) = {

    val sinCl = new Cluster(clusterSet.head.getTypes, "allElemsTogether", clusterSet.head.getElementOrdering.toSet, clusterSet.head.getNeighbourhoodTreeRepo)

    val evaluated = clusterSet.map( cl => (cl, evaluateSingle.validate(cl))).sortBy(_._2)
    println(s"---- evaluated clusters: ${evaluated.map( cl => (cl._1.getClusters.length, cl._2))}")
    val newFactors = evaluated.map(_._2).zipWithIndex.map(cl => {
      val previousCl = (cl._2 - 1) < 0 match {
        case true => evaluateSingle.validate(new Clustering(List(sinCl), clusterSet.head.getSimilarityMeasure, clusterSet.head.getElementOrdering, clusterSet.head.getSimilarityFilename))
        case false => evaluated(cl._2 - 1)._2
      }

      val consecutiveCl = (cl._2 + 1) >= evaluated.length match {
        case true => 0
        case false => evaluated(cl._2 + 1)._2
      }

      math.abs( (previousCl - evaluated(cl._2)._2)/(evaluated(cl._2)._2  - consecutiveCl))
    }).map(t => if (t.isNaN || t.isInfinity || t.isInfinite || t.isNegInfinity || t.isPosInfinity) 0.0 else t)
    println(s"---- new factors: $newFactors")

    val finalFactors = newFactors.zipWithIndex.map( f => f._1  - (factor * evaluated(f._2)._1.getClusters.length))
    println(s"---- with penalization: $finalFactors")

    //select the one with the highest score
    val cand = finalFactors.zipWithIndex.maxBy(_._1)._2
    println(s"---- selected number of clusters ${evaluated(cand)._1.getClusters.length} with id $cand")
    evaluated(cand)._1
  }

}
