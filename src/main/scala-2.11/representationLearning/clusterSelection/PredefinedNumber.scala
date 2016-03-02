package representationLearning.clusterSelection

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
    * @param elementOrdering          no used!
    * @param similarityMatrixFileName not used!
    **/
  def selectFromClusters(clusterSet: List[Set[List[String]]], elementOrdering: List[String], similarityMatrixFileName: String) = {
    clusterSet.filter( _.size == getK).head
  }
}
