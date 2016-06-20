package representationLearning.clusterComparison

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 26.02.16.
  */
abstract class AbstractClusterOverlap {

  def compare(cluster1: Clustering, cluster2: Clustering): Double

}
