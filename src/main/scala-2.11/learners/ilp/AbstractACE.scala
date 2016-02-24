package learners.ilp

import java.io.{BufferedWriter, File, FileWriter}

import learners.AbstractLearner
import logicalPrimitives.PrologDefinition
import relationalClustering.representation.KnowledgeBase
import relationalClustering.utils.Settings
import utils.RepresentationLearningSettings

import scala.io.Source
import scala.sys.process.{Process, ProcessLogger}

/**
  * Created by seb on 17.02.16.
  */
abstract class AbstractACE(protected val rootFolder: String,
                           protected val knowledgeBase: KnowledgeBase,
                           protected val latentKnowledgeBase: KnowledgeBase,
                           protected val targetPredicate: String,
                           protected val isRelation: Boolean,
                           protected val aceRootFolder: String) extends AbstractLearner {

  /** Saves the names for ACE - all to lower case*/
  protected val namesCache = collection.mutable.Map[String, String]()

  protected val basicSettings =
    """
      |load(key).
      |talking(0).
      |output_options([c45,prolog]).
      |bias(rmode).
      |classes([pos,neg]).
      |typed_language(yes).
    """.stripMargin

  /** Returns the root folder */
  protected def getRootFolder = {
    val absPath = new File(rootFolder)
    absPath.getAbsolutePath
  }

  /** Returns the knowledge base */
  protected def getOriginalKB = {
    knowledgeBase
  }

  /** Returns the name of the target concept */
  protected def getTargetPredicateName = {
    targetPredicate
  }

  /** Returns the target predicate */
  protected def getTargetPredicate = {
    latentKnowledgeBase.getPredicate(getTargetPredicateName)
  }

  /** Returns the ACE root folder */
  protected def getACERoot = {
    aceRootFolder
  }

  /** Returns the command to run */
  protected def runCommand: String

  /** Returns the additional settings that have to be set for a learner */
  protected def getAdditionalSettings: String

  /** Returns a list of files that contain rules to be extracted */
  protected def getFilesWithRules: List[String]

  /** Creates the bias for the ACE learner */
  protected def getTypes = {
    val conns = getOriginalKB.getPredicateNames.filter( _ != getTargetPredicateName ).map(getOriginalKB.getPredicate).filter( _.getRole == Settings.ROLE_HYPEREDGE).map(pred => {
      s"rmode(${pred.getName.toLowerCase}(${pred.getDomains.zipWithIndex.map(y => s"+-${y._1.capitalize}${y._2}").mkString(",")})).\n " +
        s"type(${pred.getName.toLowerCase}(${pred.getDomains.map(_.toLowerCase).mkString(",")}))."
    }).mkString("\n")

    val ann = getOriginalKB.getPredicateNames.map( getOriginalKB.getPredicate).filter( _.getRole == Settings.ROLE_ANNOTATION).map(pred => {
      s"rmode(${pred.getName.toLowerCase}(${pred.getDomains.map(y => s"+${y.capitalize}").mkString(",")})).\n" +
      s"type(${pred.getName.toLowerCase}(${pred.getDomains.map(_.toLowerCase).mkString(",")}))."
    }).mkString("\n")

    val attrs = getOriginalKB.getPredicateNames.filter( _ != getTargetPredicateName).map(getOriginalKB.getPredicate).filter( _.getRole == Settings.ROLE_ATTRIBUTE).map(pred => {
      val roles = pred.getArgumentRoles
      s"rmode(${pred.getName.toLowerCase}(${pred.getDomains.zipWithIndex.map( x => if (roles(x._2) == Settings.ARG_TYPE_ATTRIBUTE) s"+-${x._1.capitalize}${x._2}" else s"-${x._1.capitalize}${x._2}").mkString(",")})).\n" +
      s"rmode(${pred.getName.toLowerCase}(${pred.getDomainObjects.zipWithIndex.map( x => if (roles(x._2) == Settings.ARG_TYPE_ATTRIBUTE) s"#[${x._1.getElements.mkString(",")}]" else s"+-${x._1.getName.capitalize}${x._2}" )})).\n" +
      s"type(${pred.getName.toLowerCase}(${pred.getDomains.map(_.toLowerCase).mkString(",")}))."
    }).mkString("\n")

    conns + "\n" + ann + "\n" + attrs
  }

  /** Writes the ACE common settings file*/
  protected def writeSettingsFile() = {
    val writer = new BufferedWriter(new FileWriter(s"$getRootFolder/${getTargetPredicateName.toLowerCase}.s"))

    try {
      writer.write(basicSettings + "\n")
      writer.write(getAdditionalSettings + "\n")
      writer.write(s"predict(${getTargetPredicateName.toLowerCase}(${getTargetPredicate.getDomains.map( x => "+").mkString(",")},-)).\n\n")
      writer.write(getTypes + "\n")
    }
    finally {
      writer.close()
    }
  }


  /** Writes the knowledge base in a file */
  protected def writeKBToFile() = {
    val writer = new BufferedWriter(new FileWriter(s"$getRootFolder/${getTargetPredicateName.toLowerCase}.kb"))

    try {
      //add all non-target predicates
      getOriginalKB.getPredicateNames.filter( _ != getTargetPredicateName ).map(getOriginalKB.getPredicate).foreach(pred => {
        val aceName = pred.getName.toLowerCase
        namesCache(aceName) = pred.getName

        writer.write(pred.getTrueGroundings.map( x => s"$aceName(${x.map(_.toLowerCase).mkString(",")}).").mkString("\n") + "\n")
      })

      //write target predicate
      val targetAceName = getTargetPredicate.getName.toLowerCase
      namesCache(targetAceName) = getTargetPredicate.getName
      val posExamples = getTargetPredicate.getTrueGroundings

      writer.write("\n" + posExamples.map( x => s"$targetAceName(${x.map( _.toLowerCase).mkString(",")},pos).").mkString("\n") + "\n")

      //create negative examples
      val negExamples = getTargetPredicate.getDomains.map(getOriginalKB.getDomain).map(_.getElementsAsList).reduceLeft( (x,y) => { for { xs <-x; ys <- y} yield xs ::: ys}).diff(posExamples)
      writer.write("\n" + negExamples.map(x => s"$targetAceName(${x.map( _.toLowerCase).mkString(",")},neg).").mkString("\n") + "\n")
    }
    finally {
      writer.close()
    }
  }


  /** Reads the rules in a prolog syntax*/
  protected def readRulesFromFile(filename: String) = {
    val reader = Source.fromFile(s"$getRootFolder/$filename")
    val definitions = collection.mutable.Set[PrologDefinition]()
    val coverageRegex = """% (\d{1,10}\.0)/\d{1,10}\.0=(\d.\d{1,15})""".r

    try {
      var startParsing = false
      var skipCoverage = false
      var lastDefinition: PrologDefinition = null

      reader.getLines().foreach(line => {
        if (!startParsing && line.startsWith(RepresentationLearningSettings.ACE_START_PARSING_RULES)) {
          startParsing = true
        }
        else if (startParsing && line.length > 2 && !line.startsWith("%")) {
          //extract only pos definitions and skip if it is a default class
          if (line.contains("[pos]") && line.contains(":-")) {
            val newRule = new PrologDefinition(line)
            definitions += newRule
            lastDefinition = newRule
            skipCoverage = false
          }
          else {
            skipCoverage = true
          }
        }
        else if (startParsing && line.startsWith("%") && !skipCoverage) {
          //add coverage to the last definition
          val coverageRegex(absCov, relCov) = line
          lastDefinition.setAbsCoverage(absCov.toDouble.toInt)
          lastDefinition.setRelCoverage(relCov.toDouble)
        }
      })
    }
    finally {
      reader.close()
    }

    definitions.toSet
  }

  /** Extracts the prolog definitions of new predicates (file indicated with getFilesWithRules) */
  def getDefinitions = {
    getFilesWithRules.foldLeft(Set[PrologDefinition]())( (acc, file) => {
      acc ++ readRulesFromFile(file)
    })
  }

  /** Fits the model*/
  def fitModel() = {
    writeKBToFile()
    writeSettingsFile()

    (Process(Seq("echo", runCommand), new File(getRootFolder), "ACE_ILP_ROOT" -> getACERoot)
      #| Process(Seq(s"$getACERoot/bin/ace"), new File(getRootFolder), "ACE_ILP_ROOT" -> getACERoot)) ! ProcessLogger(line => println(s"TILDE says: $line"),
                                                                                                                      line => println(s"TILDE ERROR: $line"))
  }


}
