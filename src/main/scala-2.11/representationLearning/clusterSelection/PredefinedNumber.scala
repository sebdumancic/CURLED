package representationLearning.clusterSelection

/**
  * Created by seb on 09.02.16.
  */
class PredefinedNumber(protected val k: Int) extends AbstractClusterSelection {

  /** returns the number of clusters */
  def getK = {
    k
  }

  /** Selects the corresponding number of clusters */
  def selectFromClusters(clusterSet: List[Set[List[String]]]) = {
    clusterSet.filter( _.size == getK).head
  }
}
