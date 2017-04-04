package reports

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.clustering.evaluation.LabelsContainer
import relationalClustering.representation.clustering.{Cluster, Clustering}
import relationalClustering.representation.domain.{Domain, KnowledgeBase, NumericDomain, Predicate}
import representationLearning.clusterComparison.OverlapWithARI
import representationLearning.representation.ClusteringRepresentation
import vegas._
import vegas.spec.Spec

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

    render(getPredicateInteraction, "predicate1", "predicate2", "interaction", "representation", "original predicate interaction", s"$folder/predicateInteraction.html")
    render(getLatentRedundancy(folder), "ARI", "reduction", "aspect", "redundancy reduction", s"$folder/latentRedundancy.html")
  }

  protected def getVegasDataType(value: Any): Spec.Type = {
    if (value.isInstanceOf[Double] || value.isInstanceOf[Int]) {
      Quant
    }
    else {
      Nom
    }
  }

  protected def render(data: Seq[Map[String,Any]], axisX: String, axisY: String, color: String, name: String, filename: String): Unit = {
    // TODO: add automatic detection of data type for Vegas
    val plot = Vegas(name).
      withData(data).
      encodeX(axisX, getVegasDataType(data.head(axisX))).
      encodeY(axisY, getVegasDataType(data.head(axisY))).
      encodeColor(color, getVegasDataType(data.head(color))).
      mark(Bar)

    val fileWriter = new BufferedWriter(new FileWriter(filename))
    fileWriter.write(plot.html.pageHTML())
    fileWriter.close()

    val jsonFile = new BufferedWriter(new FileWriter(s"$filename.json"))
    jsonFile.write(plot.toJson)
    jsonFile.close()
  }

  protected def render(data: Seq[Map[String,Any]], axisX: String, axisY: String, color: String, row: String, name: String, filename: String): Unit = {
    val plot = Vegas(name).
      withData(data).
      encodeX(axisX, getVegasDataType(data.head(axisX))).
      encodeY(axisY, getVegasDataType(data.head(axisY))).
      encodeColor(color, getVegasDataType(data.head(color))).
      encodeRow(row, Nom).
      mark(Bar)

    val fileWriter = new BufferedWriter(new FileWriter(filename))
    fileWriter.write(plot.html.pageHTML())
    fileWriter.close()

    val jsonFile = new BufferedWriter(new FileWriter(s"$filename.json"))
    jsonFile.write(plot.toJson)
    jsonFile.close()
  }

  protected def getNumberOfTrueGroundings: Seq[Map[String,Any]] = {

    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p =>  {
      val denominator = p.getDomainObjects.foldLeft(1)((acc, dom) => dom match {
        case x: NumericDomain => acc * 1
        case x: Domain => acc * x.getElements.size
      }).toDouble/p.getDomains.zipWithIndex.foldLeft(0.0)((acc, elem) => acc*(elem._2 + 1))
      Map("predicate" -> p.getName, "count" -> p.getTrueGroundings.size.toDouble/denominator, "representation" -> "original", "domains" -> p.getDomains)
    })
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Seq[Map[String, Any]]())((acc, cl) => {
      acc ++ cl.getClusters.map( p => {
        val denominator = p.getTypes.foldLeft(1)((acc, dom) => acc * kb.getDomain(dom).getElements.size).toDouble/p.getTypes.zipWithIndex.foldLeft(0.0)((acc, elem) => acc*(elem._2 + 1))
        Map("predicate" -> p.getClusterName, "count" -> p.getSize.toDouble/denominator, "representation" -> "latent", "domains" -> p.getTypes)
      })}).toList

    originalPreds ++ latentPreds
  }

  protected def getPurity: Seq[Map[String,Any]] = {
    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p => Map("predicate" -> p.getName, "purity" -> getPredicatePurity(p), "representation" -> "original", "domains" -> p.getDomains, "instances" -> p.getTrueGroundings.size))
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Seq[Map[String, Any]]())((acc, cl) => {
      acc ++ cl.getClusters.map(p => Map("predicate" -> p.getClusterName, "purity" -> getClusterPurity(p), "representation" -> "latent", "domains" -> p.getTypes, "instances" -> p.getSize))
    }).toList

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
    kb.getPredicateNames.map(kb.getPredicate).combinations(2).foldLeft(Map[(Predicate,Predicate), Double]())((acc, pComb) => {
      val p1 = pComb.head
      val p2 = pComb(1)
      var denominator = 0

      if (p1.getDomains.intersect(p2.getDomains).isEmpty) {
        acc + ((p1, p2) -> 0.0)
      }
      else{
        acc + ( (p1, p2) -> p1.getTrueGroundings.foldLeft(0.0)((acc_1, p1gr) => {
          acc_1 + p2.getTrueGroundings.foldLeft(0.0)((acc_2, p2gr) => {
            denominator = denominator + 1
            if (p1gr.exists(el => p2gr.contains(el))) {
              acc_2 + 1.0
            }
            else {
              acc_2
            }
          })
        })/denominator)
      }
    }).flatMap(item => Seq(Map("predicate1" -> item._1._1, "predicate2" -> item._1._2, "interaction" -> item._2, "representation" -> "original", "count1" -> item._1._1.getTrueGroundings.size, "count2" -> item._1._2.getTrueGroundings.size),
                           Map("predicate2" -> item._1._1, "predicate1" -> item._1._2, "interaction" -> item._2, "representation" -> "original", "count2" -> item._1._1.getTrueGroundings.size, "count1" -> item._1._2.getTrueGroundings.size))).toList
  }

  protected def getLatentPredicateInteraction: Seq[Map[String,Any]] = {
    latentRepresentation.getClusterings.foldLeft(List[Cluster]())((acc, clustrep) => acc ++ clustrep.getClusters).combinations(2).foldLeft(Map[(Cluster,Cluster), Double]())((acc, clComb) => {
      val cl1 = clComb.head
      val cl2 = clComb(1)
      var denominator = 0

      if (cl1.getTypes.intersect(cl2.getTypes).isEmpty) {
        acc + ((cl1, cl2) -> 0.0)
      }
      else {
        acc + ((cl1, cl2) -> cl1.getInstances.foldLeft(0.0)((acc_1, cl1gr) => {
          acc_1 + cl2.getInstances.foldLeft(0.0)((acc_2, cl2gr) => {
            denominator = denominator + 1
            if (cl1gr.exists(el => cl2gr.contains(el))) {
              acc_2 + 1.0
            }
            else {
              acc_2
            }
          })
        })/denominator)
      }
    }).toList.flatMap(item => Seq(Map("predicate1" -> item._1._1.getClusterName, "predicate2" -> item._1._2.getClusterName, "interaction" -> item._2, "representation" -> "latent", "count1" -> item._1._1.getSize, "count2" -> item._1._2.getSize),
                                  Map("predicate2" -> item._1._1.getClusterName, "predicate1" -> item._1._2.getClusterName, "interaction" -> item._2, "representation" -> "latent", "count2" -> item._1._1.getSize, "count1" -> item._1._2.getSize)))
  }

  protected def getLatentRedundancy(folder: String): Seq[Map[String,Any]] = {
    val distinctTypes = latentRepresentation.getClusterings.map(_.getTypes)
    val featureDenominator = latentRepresentation.getClusterings.foldLeft(0)((acc, cl) => acc + cl.getClusters.length).toDouble
    val factDenominator = latentRepresentation.getClusterings.foldLeft(0)((acc, cl) => acc + cl.getClusters.foldLeft(0)((acc_i, p) => acc_i + p.getSize)).toDouble
    val comparator = new OverlapWithARI(folder)

    List(0.9, 0.8, 0.7, 0.6, 0.5).foldLeft(Seq[Map[String,Any]]())((acc, ari) => {
      var clusterings = List[Clustering]()

      distinctTypes.foreach(dt => {
        val currentClusterings = latentRepresentation.getClusterings.filter(_.getTypes == dt).toList
        val overlaps = collection.mutable.Map[Clustering, List[Clustering]]()

        (0 until currentClusterings.size - 1).foreach(cl1ind => {
          val overlapping = (cl1ind until currentClusterings.size).foldLeft(List[Clustering]())((acc, cl2ind) => {
            if (comparator.compare(currentClusterings(cl1ind), currentClusterings(cl2ind)) > ari) {
              acc :+ currentClusterings(cl2ind)
            }
            else {
              acc
            }
          })
          if (overlapping.nonEmpty) {
            overlaps(currentClusterings(cl1ind)) = overlapping
          }
        })
        val rejected = overlaps.values.flatten.toSet
        clusterings = clusterings ++ currentClusterings.filterNot(cl => rejected.contains(cl))
      })

      acc ++ Seq(Map("ARI" -> ari, "reduction" -> clusterings.toSet.foldLeft(0)((acc, cl) => acc + cl.getClusters.length)/featureDenominator, "aspect" -> "feature"),
                 Map("ARI" -> ari, "reduction" -> clusterings.toSet.foldLeft(0)((acc, cl) => acc + cl.getClusters.foldLeft(0)((acc_i, p) => acc_i + p.getSize))/factDenominator, "aspect" -> "fact"))
    })
  }

}
