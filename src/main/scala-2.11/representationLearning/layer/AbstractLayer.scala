package representationLearning.layer

import java.io.{BufferedWriter, FileWriter}

import utils.ClusterFactFormat

/**
  * Created by seb on 29.02.16.
  */
abstract class AbstractLayer(protected val rootFolder: String,
                             protected val outputName: String,
                             protected val maxClusters: Int,
                             protected val asFeature: Boolean) {

  protected val headerFile = new BufferedWriter(new FileWriter(s"$getRoot/$getOutputName.def"))
  protected val declarationsFile = new BufferedWriter(new FileWriter(s"$getRoot/$getOutputName.decl"))
  protected val kbFile = new BufferedWriter(new FileWriter(s"$getRoot/$getOutputName.kb"))

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
    s"$getRoot/$getOutputName.decl"
  }

  /** Returns the path to the knowledge base file */
  def getKBName = {
    s"$getRoot/$getOutputName.kb"
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
    * @param clusters a set of clusterings (set of lists)
    * @param domains  domains of the clusterings
    **/
  protected def writeFiles(clusters: Set[Set[List[String]]], domains: List[String]) = {
    clusters.zipWithIndex.foreach(clustering => {

      if (asFeature) {
        // if cluster identity if a feature, the clustering needs to have only on definition/declaration
        getHeaderFile.write(s"${formatSingleDefinition(domains, clustering._2, 0)}\n")
        getDeclFile.write(s"${formatSingleDeclaration(domains, clustering._2, 0)}\n")
      }

      clustering._1.zipWithIndex.foreach(clust => {
        if (!asFeature) {
          // if target format is not feature-format, each cluster needs its own definition
          getHeaderFile.write(s"${formatSingleDefinition(domains, clustering._2, clust._2)}\n")   //s"Cluster_${domains.mkString("_")}${clust._2 + (clustering._2 * maxClusters)}(${domains.mkString(",")})\n")
          getDeclFile.write(s"${formatSingleDeclaration(domains, clustering._2, clust._2)}\n") //s"Cluster_${domains.mkString("_")}${clust._2 + (clustering._2 * maxClusters)}(${domains.map(x => "name").mkString(",")})\n")
        }
        getKBFile.write(clust._1.map(elem => formatSingleFact(domains, clustering._2, clust._2, elem)).mkString("\n") + "\n") //s"Cluster_${domains.mkString("_")}${clust._2 + (clustering._2 * maxClusters)}(${elem.replace(":", ",")})"
      })

      getHeaderFile.write("\n")
      getDeclFile.write("\n")
      getKBFile.write("\n")
    })

    getHeaderFile.flush()
    getDeclFile.flush()
    getKBFile.flush()
  }

  /** Method to build the layer
    *
    * @return (header file name, kb file name, declarations filename)
    **/
  def build(): (String, String, String)
}
