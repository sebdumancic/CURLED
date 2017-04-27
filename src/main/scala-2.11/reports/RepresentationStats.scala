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
      render(getEntropy, "predicate", "purity", "representation", "class purity", s"$folder/labelEntropyInstance.html")
      render(getEntropyJoined, "predicate", "purity", "representation", "class purity", s"$folder/labelEntropyJoined.html")
      render(getPositionWiseEntropy, "position", "purity", "representation", "position-wise purity", s"$folder/labelEntropyRelations.html")
    }

    render(getPredicateInteraction, "predicate1", "predicate2", "interaction", "predicate interaction", s"$folder/predicateInteraction.html", List(0.0, 1.0), List("white", "red"))
    render(getPredicateInterConnectivity, "predicate1", "predicate2", "interaction", "predicate interaction", s"$folder/interactionStrength.html", List(0.0, 1.0), List("white", "red"))
    //render(getLatentRedundancy(folder), "ARI", "reduction", "aspect", "redundancy reduction", s"$folder/latentRedundancy.html")
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

  protected def render(data: Seq[Map[String,Any]], axisX: String, axisY: String, color: String, name: String, filename: String, colorDomain: List[Double], colorRange: List[String]): Unit = {
    val plot = Vegas(name).
      withData(data).
      encodeX(axisX, getVegasDataType(data.head(axisX))).
      encodeY(axisY, getVegasDataType(data.head(axisY))).
      encodeColor(color, getVegasDataType(data.head(color)), scale = Scale(domainValues = colorDomain, rangeNominals = colorRange)).
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
      val denominator = p.getDomainObjects.foldLeft(1.0)((acc, dom) => dom match {
        case x: NumericDomain => acc * 1
        case x: Domain => acc * x.getElements.size
      })/p.getDomains.zipWithIndex.foldLeft(1.0)((acc, elem) => acc*(elem._2 + 1))
      Map("predicate" -> p.getName, "proportion" -> p.getTrueGroundings.size.toDouble/denominator, "count" -> p.getTrueGroundings.size, "representation" -> "original", "domains" -> p.getDomains)
    })
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Seq[Map[String, Any]]())((acc, cl) => {
      acc ++ cl.getClusters.map( p => {
        val denominator = p.getTypes.foldLeft(1.0)((acc, dom) => acc * kb.getDomain(dom).getElements.size)/p.getTypes.zipWithIndex.foldLeft(1.0)((acc, elem) => acc*(elem._2 + 1))
        Map("predicate" -> p.getClusterName, "proportion" -> p.getSize.toDouble/denominator, "count" -> p.getSize.toDouble, "representation" -> "latent", "domains" -> p.getTypes)
      })}).toList

    originalPreds ++ latentPreds
  }

  protected def getEntropy: Seq[Map[String,Any]] = {
    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).filter(p => p.getDomains.length == 1).map(p => {
      val denominator = p.getDomainObjects.foldLeft(1.0)((acc, dom) => dom match {
        case x: NumericDomain => acc * 1
        case x: Domain => acc * x.getElements.size
      })/p.getDomains.zipWithIndex.foldLeft(1.0)((acc, elem) => acc*(elem._2 + 1))
      Map("predicate" -> p.getName, "purity" -> getPredicateEntropy(p), "representation" -> "original", "domains" -> p.getDomains, "proportion" -> p.getTrueGroundings.size.toDouble/denominator, "count" -> p.getTrueGroundings.size)
    }).filterNot(item => item("purity") == -1.0)
    val latentPreds = latentRepresentation.getClusterings.filter(p => p.getTypes.length == 1).foldLeft(Seq[Map[String, Any]]())((acc, cl) => {
      val denominator = cl.getTypes.foldLeft(1.0)((acc, dom) => acc * kb.getDomain(dom).getElements.size)/cl.getTypes.zipWithIndex.foldLeft(1.0)((acc, elem) => acc*(elem._2 + 1))
      acc ++ cl.getClusters.map(p => Map("predicate" -> p.getClusterName, "purity" -> getClusterEntropy(p), "representation" -> "latent", "domains" -> p.getTypes, "proportion" -> p.getSize.toDouble/denominator, "count" -> p.getSize))
    }).toList.filterNot(item => item("purity") == -1.0)

    originalPreds ++ latentPreds
  }

  protected def getPositionWiseEntropy: Seq[Map[String,Any]] = {
    val originalPres = kb.getPredicateNames.map(kb.getPredicate).filter(p => p.getDomains.length > 1).flatMap(p => {
      p.getDomains.indices.foldLeft(Seq[Map[String,Any]]())((acc, pos) => {
        acc :+ Map("predicate" -> p.getName, "purity" -> getPredicateEntropy(p, pos), "position" -> pos, "representation" -> "original", "count" -> p.getTrueGroundings.size)
      })
    }).filterNot(item => item("purity") == -1.0)
    val latentPreds = latentRepresentation.getClusterings.filter(_.getTypes.length > 1).foldLeft(Seq[Map[String,Any]]())((acc, cl) => {
      acc ++ cl.getClusters.flatMap(p => {
        p.getTypes.indices.foldLeft(Seq[Map[String,Any]]())((acc_i, pos) => {
          acc_i :+ Map("predicate" -> p.getClusterName, "purity" -> getClusterEntropy(p, pos), "position" -> pos, "representation" -> "latent", "count" -> p.getSize)
        })
      })
    }).filterNot(item => item("purity") == -1.0)

    originalPres ++ latentPreds
  }

  protected def getPredicateEntropy(predicate: Predicate, position: Int = 0): Double = {
    if (predicate.getDomains(position) != domain) {
      -1.0
    }
    else {
      val labels = predicate.getTrueGroundings.toList.map(gr => gr(position)).map(gr => labelsContainer.getLabel(gr))
      val labelProbs = labels.distinct.map(l => (l, labels.count(_ == l).toDouble/labels.length))
      assert(!labelProbs.exists(_._2 > 1.0))
      labelProbs.foldLeft(0.0)((acc, labp) => acc - (labp._2 * math.log10(labp._2)))
    }
  }

  protected def getClusterEntropy(cluster: Cluster, position: Int = 0): Double = {
    if (cluster.getTypes(position) != domain){
      -1.0
    }
    else {
      val labels = cluster.getInstances.toList.map(gr => gr(position)).map(gr =>labelsContainer.getLabel(gr))
      val labelProbs = labels.distinct.map(l => (l, labels.count(_ == l).toDouble/labels.length))
      assert(!labelProbs.exists(_._2 > 1.0))
      labelProbs.foldLeft(0.0)((acc, labp) => acc - (labp._2 * math.log10(labp._2)))
    }
  }

  protected def getEntropyJoined: Seq[Map[String,Any]] = {
    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p => {
      val denominator = p.getDomainObjects.foldLeft(1.0)((acc, dom) => dom match {
        case x: NumericDomain => acc * 1
        case x: Domain => acc * x.getElements.size
      })/p.getDomains.zipWithIndex.foldLeft(1.0)((acc, elem) => acc*(elem._2 + 1))
      Map("predicate" -> p.getName, "purity" -> getPredicateEntropyJoined(p), "representation" -> "original", "domains" -> p.getDomains, "proportion" -> p.getTrueGroundings.size.toDouble/denominator, "count" -> p.getTrueGroundings.size)
    }).filterNot(item => item("purity") == -1.0)
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Seq[Map[String, Any]]())((acc, cl) => {
      val denominator = cl.getTypes.foldLeft(1.0)((acc, dom) => acc * kb.getDomain(dom).getElements.size)/cl.getTypes.zipWithIndex.foldLeft(1.0)((acc, elem) => acc*(elem._2 + 1))
      acc ++ cl.getClusters.map(p => Map("predicate" -> p.getClusterName, "purity" -> getClusterEntropyJoined(p), "representation" -> "latent", "domains" -> p.getTypes, "proportion" -> p.getSize.toDouble/denominator, "count" -> p.getSize))
    }).toList.filterNot(item => item("purity") == -1.0)

    originalPreds ++ latentPreds
  }

  protected def getPredicateEntropyJoined(predicate: Predicate): Double = {
    val matchingDomainsIds = predicate.getDomains.zipWithIndex.filter(_._1 == domain).map(_._2)

    if (matchingDomainsIds.isEmpty) {
      -1.0
    }
    else {
      val labels = predicate.getTrueGroundings.toList.map(gr => gr.zipWithIndex.filter(it => matchingDomainsIds.contains(it._2))).flatMap(gr => gr.map(it => labelsContainer.getLabel(it._1)))
      val labelProbs = labels.distinct.map(l => (l, labels.count(_ == l).toDouble/labels.length))
      labelProbs.foldLeft(0.0)((acc, labp) => acc - (labp._2 * math.log10(labp._2)))
    }
  }

  protected def getClusterEntropyJoined(cluster: Cluster): Double = {
    val matchingDomainIds = cluster.getTypes.zipWithIndex.filter(_._1 == domain).map(_._2)

    if (matchingDomainIds.isEmpty) {
      -1.0
    }
    else {
      val labels = cluster.getInstances.toList.map(gr => gr.zipWithIndex.filter(it => matchingDomainIds.contains(it._2))).flatMap(gr => gr.map(it => labelsContainer.getLabel(it._1)))
      val labelProbs = labels.distinct.map(l => (l, labels.count(_ == l).toDouble/labels.length))
      labelProbs.foldLeft(0.0)((acc, labp) => acc - (labp._2 * math.log10(labp._2)))
    }
  }

  protected def getPredicateInteraction: Seq[Map[String,Any]] = {
    getOriginalPredicateInteraction ++ getLatentPredicateInteraction
  }

  protected def countPairwiseInteraction(p1: Seq[List[String]], p2: Seq[List[String]]): Double = {
    var denominator = 0.0

    (0 until p1.size - 1).foldLeft(0.0)((acc, p1ind) => {
      acc + (p1ind until p2.size).foldLeft(0.0)((acc_i, p2ind) => {
        val ground1 = p1(p1ind)
        val ground2 = p2(p2ind)
        denominator += 1.0

        if (ground1.exists(elem => ground2.contains(elem))){
          acc_i + 1
        }
        else {
          acc_i
        }
      })
    })/denominator
  }

  protected def countMatchedGroundings(p1: Seq[List[String]], p2: Seq[List[String]]): Double = {
    val denominator = p1.length + p2.length

    (p1.foldLeft(0.0)((acc, p1gr) => {
      if (p2.exists(p2gr => p1gr.exists(elem => p2gr.contains(elem)))) {
        acc + 1.0
      }
      else {
        acc
      }
    }) + p2.foldLeft(0.0)((acc, p2gr) => {
      if (p1.exists(p1gr => p2gr.exists(elem => p1gr.contains(elem)))) {
        acc + 1
      }
      else {
        acc
      }
    }))/denominator
  }

  protected def getOriginalPredicateInteraction: Seq[Map[String,Any]] = {
    kb.getPredicateNames.map(kb.getPredicate).combinations(2).foldLeft(Map[(Predicate,Predicate), Double]())((acc, pComb) => {
      val p1 = pComb.head
      val p2 = pComb(1)

      if (p1.getDomains.intersect(p2.getDomains).isEmpty) {
        acc
      }
      else{
        acc + ( (p1, p2) -> countMatchedGroundings(p1.getTrueGroundings.toSeq, p2.getTrueGroundings.toSeq))
      }
    }).flatMap(item => Seq(Map("predicate1" -> item._1._1.getName, "predicate2" -> item._1._2.getName, "interaction" -> item._2, "representation" -> "original", "count1" -> item._1._1.getTrueGroundings.size, "count2" -> item._1._2.getTrueGroundings.size))).toList
  }

  protected def getLatentPredicateInteraction: Seq[Map[String,Any]] = {
    latentRepresentation.getClusterings.foldLeft(List[Cluster]())((acc, clustrep) => acc ++ clustrep.getClusters).combinations(2).foldLeft(Map[(Cluster,Cluster), Double]())((acc, clComb) => {
      val cl1 = clComb.head
      val cl2 = clComb(1)

      if (cl1.getTypes.intersect(cl2.getTypes).isEmpty) {
        acc
      }
      else {
        acc + ((cl1, cl2) -> countMatchedGroundings(cl1.getInstances.toSeq, cl2.getInstances.toSeq))
      }
    }).toList.flatMap(item => Seq(Map("predicate1" -> item._1._1.getClusterName, "predicate2" -> item._1._2.getClusterName, "interaction" -> item._2, "representation" -> "latent", "count1" -> item._1._1.getSize, "count2" -> item._1._2.getSize)))
  }

  protected def getPredicateInterConnectivity: Seq[Map[String,Any]] = {
    getOriginalPredicateInterConnectivity ++ getLatentPredicateInterConnectivity
  }

  protected def getOriginalPredicateInterConnectivity: Seq[Map[String,Any]] = {
    kb.getPredicateNames.map(kb.getPredicate).combinations(2).foldLeft(Map[(Predicate,Predicate), Double]())((acc, pComb) => {
      val p1 = pComb.head
      val p2 = pComb(1)

      if (p1.getDomains.intersect(p2.getDomains).isEmpty) {
        acc
      }
      else{
        acc + ( (p1, p2) -> countPairwiseInteraction(p1.getTrueGroundings.toSeq, p2.getTrueGroundings.toSeq))
      }
    }).flatMap(item => Seq(Map("predicate1" -> item._1._1.getName, "predicate2" -> item._1._2.getName, "interaction" -> item._2, "representation" -> "original", "count1" -> item._1._1.getTrueGroundings.size, "count2" -> item._1._2.getTrueGroundings.size))).toList
  }

  protected def getLatentPredicateInterConnectivity: Seq[Map[String,Any]] = {
    latentRepresentation.getClusterings.foldLeft(List[Cluster]())((acc, clustrep) => acc ++ clustrep.getClusters).combinations(2).foldLeft(Map[(Cluster,Cluster), Double]())((acc, clComb) => {
      val cl1 = clComb.head
      val cl2 = clComb(1)

      if (cl1.getTypes.intersect(cl2.getTypes).isEmpty) {
        acc
      }
      else {
        acc + ((cl1, cl2) -> countPairwiseInteraction(cl1.getInstances.toSeq, cl2.getInstances.toSeq))
      }
    }).toList.flatMap(item => Seq(Map("predicate1" -> item._1._1.getClusterName, "predicate2" -> item._1._2.getClusterName, "interaction" -> item._2, "representation" -> "latent", "count1" -> item._1._1.getSize, "count2" -> item._1._2.getSize)))
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
