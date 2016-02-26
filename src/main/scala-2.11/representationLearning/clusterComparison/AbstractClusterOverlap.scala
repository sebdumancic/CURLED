package representationLearning.clusterComparison

/**
  * Created by seb on 26.02.16.
  */
abstract class AbstractClusterOverlap {

  def compare(cluster1: Set[List[String]], cluster2: Set[List[String]]): Double

}
