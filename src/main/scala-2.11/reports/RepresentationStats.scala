package reports

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.clustering.evaluation.LabelsContainer
import relationalClustering.representation.clustering.Cluster
import relationalClustering.representation.domain.{KnowledgeBase, Predicate}
import representationLearning.representation.ClusteringRepresentation
import vegas._

/**
  * Created by seb on 29.03.17.
  */
class RepresentationStats(protected val kb: KnowledgeBase,
                          protected val latentRepresentation: ClusteringRepresentation,
                          protected val labelsContainer: LabelsContainer,
                          protected val domain: String) {

  def generateReport(folder: String): Unit = {

    render(getNumberOfTrueGroundings, "predicate", "count", "representation", "number of groundings", s"$folder/numberOfGroundings.html")
    if (labelsContainer != null) {
      render(getPurity, "predicate", "purity", "representation", "class purity", s"$folder/classPurity.html")
    }
  }

  protected def render(data: Seq[Map[String,Any]], axisX: String, axisY: String, color: String, name: String, filename: String): Unit = {
    val plot = Vegas(name).
      withData(data).
      encodeX(axisX, Ord).
      encodeY(axisY, Quant).
      encodeColor(color, Nom).
      mark(Bar)

    val fileWriter = new BufferedWriter(new FileWriter(filename))
    fileWriter.write(plot.html.pageHTML())
    fileWriter.close()

    val jsonFile = new BufferedWriter(new FileWriter(s"$filename.json"))
    jsonFile.write(plot.toJson)
    jsonFile.close()
  }

  protected def getNumberOfTrueGroundings: Seq[Map[String,Any]] = {

    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p => (p.getName, p.getTrueGroundings.size)).map(item => {
      Map("predicate" -> item._1, "count" -> item._2, "representation" -> "original")
    })
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Map[String, Int]())((acc, cl) => {
      acc ++ cl.getClusters.foldLeft(Map[String, Int]())((acc_i, p) => acc_i + (p.getClusterName -> p.getSize))}).toList.map(item => {
      Map("predicate" -> item._1, "count" -> item._2, "representation" -> "latent")
    })

    originalPreds ++ latentPreds
  }

  protected def getPurity: Seq[Map[String,Any]] = {
    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p => (p.getName, getPredicatePurity(p )))map(item => {
      Map("predicate" -> item._1, "purity" -> item._2, "representation" -> "original")
    })
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Map[String, Double]())((acc, cl) => {
      acc ++ cl.getClusters.foldLeft(Map[String, Double]())((acc_i, p) => acc_i + (p.getClusterName -> getClusterPurity(p)))
    }).toList.map(item => Map("predicate" -> item._1, "purity" -> item._2, "representation" -> "latent"))

    originalPreds ++ latentPreds
  }

  protected def getPredicatePurity(predicate: Predicate): Double = {
    val matchingDomainsIds = predicate.getDomains.zipWithIndex.filter(_._1 == domain).map(_._2)

    if (matchingDomainsIds.isEmpty) {
      0.0
    }
    else {
      val labels = predicate.getTrueGroundings.toList.map(gr => gr.zipWithIndex.filter(it => matchingDomainsIds.contains(it._2))).flatMap(gr => gr.map(it => labelsContainer.getLabel(it._1)))
      labels.distinct.map(l => (l, labels.count(_ == l).toDouble/labels.length)).maxBy(_._2)._2
    }
  }

  protected def getClusterPurity(cluster: Cluster): Double = {
    val matchingDomainIds = cluster.getTypes.zipWithIndex.filter(_._1 == domain).map(_._2)

    if (matchingDomainIds.isEmpty) {
      0.0
    }
    else {
      val labels = cluster.getInstances.toList.map(gr => gr.zipWithIndex.filter(it => matchingDomainIds.contains(it._2))).flatMap(gr => gr.map(it => labelsContainer.getLabel(it._1)))
      labels.distinct.map(l => (l, labels.count(_ == l).toDouble/labels.length)).maxBy(_._2)._2
    }
  }

  protected def getPredicateInteraction: Seq[Map[String,Any]] = {
    getOriginalPredicateInteraction ++ getLatentPredicateInteraction
  }

  protected def getOriginalPredicateInteraction: Seq[Map[String,Any]] = {
    kb.getPredicateNames.map(kb.getPredicate).combinations(2).foldLeft(Map[(String,String), Double]())((acc, pComb) => {
      val p1 = pComb.head
      val p2 = pComb(1)
      val denominator = List(p1.getTrueGroundings.size,p2.getTrueGroundings.size).max

      if (p1.getDomains.intersect(p2.getDomains).isEmpty) {
        acc + ((p1.getName, p2.getName) -> 0.0)
      }
      else{
        acc + ( (p1.getName, p2.getName) -> p1.getTrueGroundings.foldLeft(0.0)((acc_1, p1gr) => {
          acc_1 + p2.getTrueGroundings.foldLeft(0.0)((acc_2, p2gr) => {
            if (p1gr.exists(el => p2gr.contains(el))) {
              acc_2 + 1.0
            }
            else {
              acc_2
            }
          })
        })/denominator)
      }
    }).map(item => Map("predicate1" -> item._1._1, "predicate2" -> item._1._2, "interaction" -> item._2, "representation" -> "original")).toList
  }

  protected def getLatentPredicateInteraction: Seq[Map[String,Any]] = {
    latentRepresentation.getClusterings.foldLeft(List[Cluster]())((acc, clustrep) => acc ++ clustrep.getClusters).combinations(2).foldLeft(Map[(String,String), Double]())((acc, clComb) => {
      val cl1 = clComb.head
      val cl2 = clComb(1)
      val denominator = List(cl1.getSize, cl2.getSize).max

      if (cl1.getTypes.intersect(cl2.getTypes).isEmpty) {
        acc + ((cl1.getClusterName, cl2.getClusterName) -> 0.0)
      }
      else {
        acc + ((cl1.getClusterName, cl2.getClusterName) -> cl1.getInstances.foldLeft(0.0)((acc_1, cl1gr) => {
          acc_1 + cl2.getInstances.foldLeft(0.0)((acc_2, cl2gr) => {
            if (cl1gr.exists(el => cl2gr.contains(el))) {
              acc_2 + 1.0
            }
            else {
              acc_2
            }
          })
        })/denominator)
      }
    }).toList.map(item => Map("predicate1" -> item._1._1, "predicate2" -> item._1._2, "interaction" -> item._2, "representation" -> "latent"))
  }

}
