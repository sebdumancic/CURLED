package representationLearning.clusterSelection

import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 09.02.16.
  */
class PredefinedNumber(protected val k: Int) extends AbstractClusterSelection {

  /** returns the number of clusters */
  def getK = {
    k
  }

  /** Selects the corresponding number of clusters
    *
    * @param clusterSet               a set of clusterings
    **/
  def selectFromClusters(clusterSet: List[Clustering]) = {
    clusterSet.filter(_.getClusters.length == getK).head
  }
}
