package representationLearning.representation

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.bagComparison.ChiSquaredDistance
import relationalClustering.bagComparison.bagCombination.UnionCombination
import relationalClustering.neighbourhood.{NeighbourhoodGraph, NodeRepository}
import relationalClustering.representation.clustering.Clustering
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Created by seb on 20.06.16.
  */
class ClusteringRepresentation(protected val clusterings: Set[Clustering]) {

  /** Writes new representation to files
    *
    * @param filename name of the files for *.db, *.def and *.dcl
    * */
  def write(filename: String, folder: String) = {
    val KBfile = new BufferedWriter(new FileWriter(s"$folder/$filename.db"))
    val defFile = new BufferedWriter(new FileWriter(s"$folder/$filename.def"))
    val declFile = new BufferedWriter(new FileWriter(s"$folder/$filename.dcl"))

    clusterings.foreach( clust => {
      clust.printClusteringAsFacts(KBfile)
      clust.printClusteringDefinition(defFile)
      clust.printClusteringDeclaration(declFile)

      KBfile.write(sys.props("line.separator"))
      defFile.write(sys.props("line.separator"))
      declFile.write(sys.props("line.separator"))
    })

    KBfile.close()
    defFile.close()
    declFile.close()

    (s"$folder/$filename.def", s"$folder/$filename.dcl", s"$folder/$filename.db")
  }

  /** Maps the facts from a new knowledge base to the extracted representation
    *
    * @param kb knowledge base
    * @param linkage linkage for assigning to the closest cluster (average, maximal, minimal)
    * @return a set of facts
    * */
  def mapNewFacts(kb: KnowledgeBase, linkage: String = "average") = {
    val ntDepth: Int = clusterings.head.getSimilarityMeasure.getDepth
    val nodeRepo = new NodeRepository(kb)
    val ntRepo = collection.mutable.Map[(String,String), NeighbourhoodGraph]()

    //assign vertices to clusters - over all domains in clustering
    var facts = clusterings.filter(_.vertexClustering).map(_.getTypes.head).map(d => kb.getDomain(d)).foldLeft(Set[String]())( (acc, dom) => {
      // over all elements of a domain
      acc ++ dom.getElements.foldLeft(Set[String]())( (acc_i, elem) => {
        val elemNT = new NeighbourhoodGraph(elem, dom.getName, ntDepth, kb, nodeRepo)
        ntRepo((elem, dom.getName)) = elemNT

        // over all clusterings of the same type
        acc_i ++ clusterings.filter( _.vertexClustering ).filter( _.getTypes.head == dom.getName ).foldLeft(Set[String]())( (acc_ii, cl) => {
          acc_ii + s"${cl.assignToClosestCluster(List(elemNT), linkage).getClusterName}($elem)"
        })
      })
    })

    // similarity measure just to extract hyperedges in a knowledge base
    val tmpSimMsForEdges = new SimilarityNeighbourhoodTrees(kb, ntDepth, List(0.2,0.2,0.2,0.2,0.2), new ChiSquaredDistance(), new UnionCombination())

    // for each existing domain combination
    facts = facts ++ clusterings.filterNot(_.vertexClustering).map(_.getTypes).foldLeft(Set[String]())( (acc, comb) => {
      val existingEdges = tmpSimMsForEdges.getHyperEdges(comb)
      existingEdges.nonEmpty match {
        case false => acc // if no hyperedges of this type
        case true => acc ++ existingEdges.foldLeft(Set[String]())( (acc_i, edge) => {
          val ntRepresentation = edge.zip(comb).map( e => ntRepo(e._1, e._2))
          acc_i ++ clusterings.filterNot(_.vertexClustering).filter(_.getTypes == comb).foldLeft(Set[String]())( (acc_ii, cl) => {
            acc_ii + s"${cl.assignToClosestCluster(ntRepresentation, linkage).getClusterName}(${edge.mkString(",")})"
          })
        })
      }
    })

    facts
  }

}
