package representationLearning.clusterSelection

import relationalClustering.clustering.evaluation.AbstractEvaluatorModel

/**
  * Implement model based selection - applies the evaluation method to each clustering, and returns the one with the highest score
  * Created by seb on 10.02.16.
  */
class ModelBasedSelection(protected val evaluateSingle: AbstractEvaluatorModel) extends AbstractClusterSelection {

  def selectFromClusters(clusterSet: List[Set[List[String]]], elementOrdering: List[String], similarityMatrixFileName: String) = {
    val evals = clusterSet.filter( x => x.size > 1).map(cluster => new Tuple2(cluster, evaluateSingle.validate(cluster, elementOrdering, similarityMatrixFileName)))
    println(s"---- ---- ----- ModelBasedSelection::select from clusters evaluations: ${evals.map(_._2)}")

    if (evals.isEmpty) {
      clusterSet.head
    }
    else {
      evals.maxBy(_._2)._1
    }
  }
}
