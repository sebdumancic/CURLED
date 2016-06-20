package representationLearning.clusterSelection

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 09.02.16.
  */
abstract class AbstractClusterSelection {

  /** Selects the best clustering according to a certain score
    *
    * @param clusterSet               an ordered set of clusterings
    **/
  def selectFromClusters(clusterSet: List[Clustering]): Clustering
}
