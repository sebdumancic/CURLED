package representationLearning.clusterSelection

/**
  * Created by seb on 09.02.16.
  */
abstract class AbstractClusterSelection {

  /** Selects the best clustering according to a certain score */
  def selectFromClusters(clusterSet: List[Set[List[String]]]): Set[List[String]]
}
