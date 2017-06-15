package representationLearning.clusterComparison

import java.io.{BufferedWriter, File, FileWriter}

import relationalClustering.clustering.evaluation.supervised.{AdjustedRandIndex, LabelsContainer}
import relationalClustering.representation.clustering.Clustering

/**
  * Created by seb on 26.02.16.
  */
class OverlapWithARI(protected val rootFolder: String) extends AbstractClusterOverlap {

  /** Returns the root folder */
  protected def getRoot = {
    rootFolder
  }

  /** Measures the overlap between cluster using the Adjusted Rand Index
    *
    * @param cluster1 a set of clusters
    * @param cluster2 a set of clusters
    * @return adjusted rand index of two clustering
    **/
  def compare(cluster1: Clustering, cluster2: Clustering) = {
    saveAsLabels(cluster1)

    val fakeLabels = new LabelsContainer(Seq(s"$getRoot/tmp_labels.db"))
    val ari = new AdjustedRandIndex(getRoot)

    val res = ari.validate(cluster2, fakeLabels)
    cleanArtifacts()
    res
  }

  /** Saves clusters in labels format where the label is cluster ID */
  protected def saveAsLabels(cluster: Clustering) = {
    val writer = new BufferedWriter(new FileWriter(s"$getRoot/tmp_labels.db"))

    cluster.getClusters.foreach(clust => {
      writer.write(clust.getClusterFacts.mkString(sys.props("line.separator")) + sys.props("line.separator"))
    })
    writer.close()
  }

  /** cleans the artifacts produced */
  protected def cleanArtifacts() = {
    val tmpLabels = new File(s"$getRoot/tmp_labels.db")
    val groundTruth = new File(s"$getRoot/ari_ground_truth.txt")
    val script = new File(s"$getRoot/ari_script.py")

    tmpLabels.delete()
    groundTruth.delete()
    script.delete()
  }
}
