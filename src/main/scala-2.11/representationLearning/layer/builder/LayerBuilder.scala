package representationLearning.layer.builder

import java.io.{BufferedWriter, FileWriter}

import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.utils.{Helper, PredicateDeclarations}
import representationLearning.layer.AbstractLayer
import representationLearning.representation.ClusteringRepresentation

/**
  * Created by seb on 07.09.16.
  */
class LayerBuilder(protected val definedLayer: AbstractLayer,
                   protected val writingFolder: String,
                   protected val linkage: String) {

  protected val newRep: ClusteringRepresentation = definedLayer.build()

  def writeNewRepresentation(filename: String): (String,String,String) = {
    newRep.write(filename, writingFolder)
  }

  def getClusteringRepresentation: ClusteringRepresentation = {
    newRep
  }

  def writeDefinitions(minSupport: Double, maxDeviance: Double): Unit = {
    newRep.mineDefinitions(minSupport, maxDeviance, writingFolder)
  }

  def writeDefinitions(k: Int): Unit = {
    newRep.mineDefinitions(k, writingFolder)
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
