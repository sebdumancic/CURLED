package representationLearning.clusterSelection

/**
  * Created by seb on 09.02.16.
  */
abstract class AbstractClusterSelection {

  /** Selects the best clustering according to a certain score
    *
    * @param clusterSet               an ordered set of clusterings
    * @param elementOrdering          an ordering of elements in a similarity matrix
    * @param similarityMatrixFileName a file containing the similarity matrix
    **/
  def selectFromClusters(clusterSet: List[Set[List[String]]], elementOrdering: List[String], similarityMatrixFileName: String): Set[List[String]]
}
