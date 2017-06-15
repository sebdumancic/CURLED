package representationLearning.clusterSelection

import relationalClustering.clustering.evaluation.unsupervised.AbstractEvaluatorModel
import relationalClustering.representation.clustering.Clustering

/**
  * Implement model based selection - applies the evaluation method to each clustering, and returns the one with the highest score
  * Created by seb on 10.02.16.
  */
class ModelBasedSelection(protected val evaluateSingle: AbstractEvaluatorModel) extends AbstractClusterSelection {

  def selectFromClusters(clusterSet: List[Clustering]) = {
    val evals = clusterSet.filter( x => x.getClusters.length > 1).map(cluster => new Tuple2(cluster, evaluateSingle.validate(cluster)))
    println(s"---- ---- ----- ModelBasedSelection::select from clusters evaluations: ${evals.map(_._2)}")

    if (evals.isEmpty) {
      clusterSet.head
    }
    else {
      evals.maxBy(_._2)._1
    }
  }
}
