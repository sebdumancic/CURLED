package representationLearning.representation

import java.io.{BufferedWriter, File, FileWriter}

import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bagComparison.ChiSquaredDistance
import relationalClustering.bagComparison.bagCombination.UnionCombination
import relationalClustering.neighbourhood.{NeighbourhoodGraph, NodeRepository}
import relationalClustering.representation.clustering.Clustering
import relationalClustering.representation.definition.{DefinitionMinerThreshold, DefinitionMinerTopK}
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.{SimilarityNeighbourhoodTrees, SimilarityNeighbourhoodTreesOrdered}


/**
  * Created by seb on 20.06.16.
  */
class ClusteringRepresentation(protected val clusterings: Set[Clustering],
                               protected val preserveOrder: Boolean,
                               protected val aggregators: List[AbstractAggregator],
                               protected val vertexCombination: String) {

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
  def mapNewFacts(kb: KnowledgeBase, linkage: String = "maximal"): Set[String] = {
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
          val clusterSimilarities = cl.assignToClosestCluster(List(elemNT), linkage)
          val closest = clusterSimilarities.maxBy(_._2)._1
          acc_ii + s"${closest.getClusterName}($elem)"
        })
      })
    })

    // similarity measure just to extract hyperedges in a knowledge base
    val tmpSimMsForEdges = if (!preserveOrder) {
      new SimilarityNeighbourhoodTrees(kb, ntDepth, List(0.2,0.2,0.2,0.2,0.2), new ChiSquaredDistance(), new UnionCombination(), aggregators)
    }
    else {
      new SimilarityNeighbourhoodTreesOrdered(kb, ntDepth, List(0.2,0.2,0.2,0.2,0.2), new ChiSquaredDistance(), vertexCombination, aggregators)
    }

    // for each existing domain combination
    facts = facts ++ clusterings.filterNot(_.vertexClustering).map(_.getTypes).foldLeft(Set[String]())( (acc, comb) => {
      val existingEdges = tmpSimMsForEdges.getHyperEdges(comb)
      existingEdges.nonEmpty match {
        case false => acc // if no hyperedges of this type
        case true => acc ++ existingEdges.foldLeft(Set[String]())( (acc_i, edge) => {
          val ntRepresentation = edge.zip(comb).map( e => ntRepo(e._1, e._2))
          acc_i ++ clusterings.filterNot(_.vertexClustering).filter(_.getTypes == comb).foldLeft(Set[String]())( (acc_ii, cl) => {
            val clusterSimilarities = cl.assignToClosestCluster(ntRepresentation, linkage)
            val closest = clusterSimilarities.maxBy(_._2)._1
            acc_ii + s"${closest.getClusterName}(${edge.mkString(",")})"
          })
        })
      }
    })

    facts
  }

  /** Extracts the definitions of clusters by thresholding on tuple support, and saves them in the folders
    *
    * @param minSupport minimal support for DefinitionMinerThreshold
    * @param maxDeviance maximal deviance for DefinitionMinerThreshold
    * @param folder folder to save definitions in
    **/
  def mineDefinitions(minSupport: Double, maxDeviance: Double, folder: String): Unit = {
    val basePath = s"$folder/definitions"
    val directory = new File(basePath)
    directory.mkdir()

    clusterings.foreach(clustering => {

      val miner = new DefinitionMinerThreshold(clustering, minSupport, maxDeviance)
      val defs = miner.getDefinitions(clustering.getParameters)

      defs.foreach(clust => {
        val writer = new BufferedWriter(new FileWriter(s"$basePath/${clust._1}.cluster.definitions"))
        if (clust._2.nonEmpty) {
          writer.write(s"${clust._1} (${clust._2.head.getTupleContexts.head.getNumObjects} entities)\n\n${clust._2.map(_.toString()).mkString("\n\n")}\n${"*"*30}\n\n")
        }
        else {
          writer.write(s"${clust._1}\n\n \t NO DESCRIPTION!!!")
        }
        writer.close()
      })


    })
  }

  /** Extracts the definitions of clusters by extracting top K tuples, and saves them in the folders
    *
    * @param k minimal support for DefinitionMinerThreshold
    * @param folder folder to save definitions in
    **/
  def mineDefinitions(k: Int, folder: String): Unit = {
    val basePath = s"$folder/definitions"
    val directory = new File(basePath)
    directory.mkdir()

    clusterings.foreach(clustering => {
      val miner = new DefinitionMinerTopK(clustering, k)
      val defs = miner.getDefinitions(clustering.getParameters)

      defs.foreach(clust => {
        val writer = new BufferedWriter(new FileWriter(s"$basePath/${clust._1}.cluster.definitions"))
        if (clust._2.nonEmpty) {
          writer.write(s"${clust._1} (${clust._2.head.getTupleContexts.head.getNumObjects} entities)\n\n${clust._2.map(_.toString()).mkString("\n\n")}\n${"*"*30}\n\n")
        }
        else {
          writer.write(s"${clust._1}\n\n \t NO DESCRIPTION!!!")
        }
        writer.close()
      })


    })
  }

  def getClusterings: Set[Clustering] = {
    clusterings
  }

}
