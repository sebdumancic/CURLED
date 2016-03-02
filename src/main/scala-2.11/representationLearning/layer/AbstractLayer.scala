package representationLearning.layer

import java.io.{BufferedWriter, FileWriter}

/**
  * Created by seb on 29.02.16.
  */
abstract class AbstractLayer(protected val rootFolder: String,
                             protected val outputName: String) {

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

  /** Method to build the layer
    *
    * @return (header file name, kb file name, declarations filename)
    **/
  def build(): (String, String, String)
}
