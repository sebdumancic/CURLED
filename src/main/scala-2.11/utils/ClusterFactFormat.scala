package utils

/**
  * Created by seb on 02.03.16.
  */
object ClusterFactFormat {

  def asFeature(domains: List[String], clusteringId: Int, clusterId: Int, elem: String) = {
    s"Cluster_${domains.mkString("_")}$clusteringId(${elem.replace(":", ",")}, cluster_${domains.mkString("_")}$clusterId)"
  }

  def asAnnotation(domains: List[String], clusteringId: Int, clusterId: Int, onset: Int, elem: String) = {
   s"Cluster_${domains.mkString("_")}${clusterId + (onset * clusteringId)}(${elem.replace(":", ",")})"
  }

  def definitionAsFeature(domains: List[String], clusteringId: Int) = {
    s"Cluster_${domains.mkString("_")}$clusteringId(${domains.mkString(",")}, type${domains.mkString("_")}$clusteringId)"
  }

  def definitionAsAnnotation(domains: List[String], clusteringId: Int, clusterId: Int, onset: Int) = {
    s"Cluster_${domains.mkString("_")}${clusterId + (onset * clusteringId)}(${domains.mkString(",")})"
  }

  def declarationsAsFeature(domains: List[String], clusteringId: Int) = {
    s"Cluster_${domains.mkString("_")}$clusteringId(${domains.map( x => "name").mkString(",")}, attribute)"
  }

  def declarationAsAnnotation(domains: List[String], clusteringId: Int, clusterId: Int, onset: Int) = {
    s"Cluster_${domains.mkString("_")}${clusterId + (onset * clusteringId)}(${domains.map(x => "name").mkString(",")})"
  }

}
