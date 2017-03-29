package reports

import relationalClustering.clustering.evaluation.LabelsContainer
import relationalClustering.representation.clustering.Cluster
import relationalClustering.representation.domain.{KnowledgeBase, Predicate}
import representationLearning.representation.ClusteringRepresentation

/**
  * Created by seb on 29.03.17.
  */
class RepresentationStats(protected val kb: KnowledgeBase,
                          protected val latentRepresentation: ClusteringRepresentation,
                          protected val labelsContainer: LabelsContainer,
                          protected val domain: String) {

  def generateReport(folder: String): Unit = {

  }

  protected def getNumberOfTrueGroundings: (Map[String, Int], Map[String, Int]) = {

    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p => (p.getName, p.getTrueGroundings.size)).toMap
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Map[String, Int]())((acc, cl) => {
      acc ++ cl.getClusters.foldLeft(Map[String, Int]())((acc_i, p) => acc_i + (p.getClusterName -> p.getSize))})

    (originalPreds, latentPreds)
  }

  protected def getPurity: (Map[String, Double], Map[String, Double]) = {
    val originalPreds = kb.getPredicateNames.map(kb.getPredicate).map(p => (p.getName, getPredicatePurity(p ))).toMap
    val latentPreds = latentRepresentation.getClusterings.foldLeft(Map[String, Double]())((acc, cl) => {
      acc ++ cl.getClusters.foldLeft(Map[String, Double]())((acc_i, p) => acc_i + (p.getClusterName -> getClusterPurity(p)))
    })

    (originalPreds, latentPreds)
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

  def getPredicateInteraction: (Map[(String,String), Double], Map[(String,String), Double]) = {
    (getOriginalPredicateInteraction, getLatentPredicateInteraction)
  }

  def getOriginalPredicateInteraction: Map[(String,String), Double] = {
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
    })
  }

  def getLatentPredicateInteraction: Map[(String,String), Double] = {
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
    })
  }

}
