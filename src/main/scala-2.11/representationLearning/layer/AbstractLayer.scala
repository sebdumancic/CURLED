package representationLearning.layer

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.neighbourhood.NeighbourhoodGraph
import relationalClustering.representation.clustering.Clustering
import representationLearning.representation.ClusteringRepresentation
import utils.ClusterFactFormat

/**
  * Created by seb on 29.02.16.
  */
abstract class AbstractLayer(protected val rootFolder: String,
                             protected val outputName: String,
                             protected val maxClusters: Int,
                             protected val asFeature: Boolean) {

  protected val headerFile = new BufferedWriter(new FileWriter(s"$getRoot/$getOutputName.def"))
  protected val declarationsFile = new BufferedWriter(new FileWriter(s"$getRoot/$getOutputName.dcl"))
  protected val kbFile = new BufferedWriter(new FileWriter(s"$getRoot/$getOutputName.db"))
  protected val neighTreeCache = collection.mutable.Map[(String,String), NeighbourhoodGraph]()

  /** Adds a neighbourhood tree to the neighbourhood tree cache */
  protected def addToCache(key: (String,String), value: NeighbourhoodGraph) = {
      neighTreeCache(key) = value
  }

  /** Adds entire collection of neighbourhood trees to cache*/
  protected def addTreesToCache(coll: Map[(String,String), NeighbourhoodGraph]) = {
    coll.filter( item => !neighTreeCache.contains(item._1)).foreach( item => {
      addToCache(item._1, item._2)
    })
  }

  /** Returns a global neighbourhood tree cache */
  protected def getNeighTreeCache = {
    neighTreeCache.toMap
  }

  /** Returns the root folder */
  protected def getRoot = {
    rootFolder
  }

  /** Returns the output first */
  protected def getOutputName = {
    outputName
  }

  /** Returns the header file [[BufferedWriter]] */
  protected def getHeaderFile = {
    headerFile
  }

  /** Returns the declarations file [[BufferedWriter]] */
  protected def getDeclFile = {
    declarationsFile
  }

  /** Returns the path to the definition file */
  def getHeaderName = {
    s"$getRoot/$getOutputName.def"
  }

  /** Return the path to the declarations file */
  def getDeclarationsName = {
    s"$getRoot/$getOutputName.dcl"
  }

  /** Returns the path to the knowledge base file */
  def getKBName = {
    s"$getRoot/$getOutputName.db"
  }

  /** Returns the knowledge base file [[BufferedWriter]] */
  protected def getKBFile = {
    kbFile
  }

  /** Closes the files */
  protected def closeFiles() = {
    getDeclFile.close()
    getHeaderFile.close()
    getKBFile.close()
  }

  /** Closes the current */
  protected def flushFiles() = {
    getDeclFile.flush()
    getHeaderFile.flush()
    getKBFile.flush()
  }

  /** Formats single fact into a desired format
    *
    * @param domains domains of the provided clustering
    * @param clustering clustering id
    * @param cluster cluster id with regards to the clustering
    * @param elem arguments of the fact
    * */
  protected def formatSingleFact(domains: List[String], clustering: Int, cluster: Int, elem: String) = {
    asFeature match {
      case true => ClusterFactFormat.asFeature(domains, clustering, cluster, elem)
      case false => ClusterFactFormat.asAnnotation(domains, clustering, cluster, maxClusters, elem)
    }
  }

  protected def formatSingleDefinition(domains: List[String], clustering: Int, cluster: Int) = {
    asFeature match {
      case true => ClusterFactFormat.definitionAsFeature(domains, clustering)
      case false => ClusterFactFormat.definitionAsAnnotation(domains, clustering, cluster, maxClusters)
    }
  }

  protected def formatSingleDeclaration(domains: List[String], clustering: Int, cluster: Int) = {
    asFeature match {
      case true => ClusterFactFormat.declarationsAsFeature(domains, clustering)
      case false => ClusterFactFormat.declarationAsAnnotation(domains, clustering, cluster, maxClusters)
    }
  }

  /** Writes the provided clusterings in a file
    *
    * @param clusterings a set of clusterings (set of lists)
    **/
  protected def writeFiles(clusterings: Set[Clustering]) = {
    clusterings.zipWithIndex.foreach(clustering => {

      clustering._1.printClusteringAsFacts(getKBFile)
      clustering._1.printClusteringDefinition(getHeaderFile)
      clustering._1.printClusteringDeclaration(getDeclFile)

      getHeaderFile.write(s"${sys.props("line.separator")}")
      getDeclFile.write(s"${sys.props("line.separator")}")
      getKBFile.write(s"${sys.props("line.separator")}")
    })

    getHeaderFile.flush()
    getDeclFile.flush()
    getKBFile.flush()
  }

  /** Method to build the layer
    *
    * @return new representation obtained with clustering
    **/
  def build(): ClusteringRepresentation
}
