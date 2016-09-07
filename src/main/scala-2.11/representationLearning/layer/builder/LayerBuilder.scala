package representationLearning.layer.builder

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.utils.{Helper, PredicateDeclarations}
import representationLearning.layer.AbstractLayer

/**
  * Created by seb on 07.09.16.
  */
class LayerBuilder(protected val definedLayer: AbstractLayer,
                   protected val writingFolder: String,
                   protected val linkage: String) {

  protected val newRep = definedLayer.build()
  protected var newRepresentationHeaderFilename: String = _
  protected var newRepresentationDeclarationsFilename: String = _
  protected var newRepresentationKBFilename: String = _


  def writeNewRepresentation(filename: String) = {
    (newRepresentationHeaderFilename, newRepresentationKBFilename, newRepresentationKBFilename) = newRep.write(filename, writingFolder)
  }

  def getNewRepresentationFiles = {
    newRepresentationKBFilename == null match {
      case true => throw new Exception(s"LayerBuilder : new representation not written to files yet!")
      case false => (newRepresentationHeaderFilename, newRepresentationDeclarationsFilename, newRepresentationKBFilename)
    }
  }

  def mapFacts(factFilename: String, headerFile: String, predicateDeclarations: PredicateDeclarations) = {
    val latentKB = new KnowledgeBase(List(factFilename), Helper.readFile(headerFile).mkString("\n"), predicateDeclarations)
    newRep.mapNewFacts(latentKB, linkage)
  }

  def mapAndWriteFacts(factFilename: String, headerFile: String, predicateDeclarations: PredicateDeclarations, outputFilename: String) = {
    val latentFacts = mapFacts(factFilename, headerFile, predicateDeclarations)
    val LKBOutput = new BufferedWriter(new FileWriter(s"$writingFolder/$outputFilename"))

    try {
      LKBOutput.write(latentFacts.toList.sorted.mkString(sys.props("line.separator")))
    }
    catch {
      case e: Exception => throw new Exception(s"LayerBuilder:mapAndWriteFacts :: couldn't print latent fact to file $outputFilename")
    }
    finally  {
      LKBOutput.close()
    }


  }



}
