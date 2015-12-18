package deepSRL.tilde

import java.io.{File, FileWriter}

import deepSRL.basics.KnowledgeBase
import deepSRL.utils.Helper

import scala.sys.process.{Process, ProcessLogger}

/**
 * This class implements a complete TILDE wrapper
 *
 * Created by seb on 24.09.15.
 */
class TILDELearner(private val kbase: KnowledgeBase,
                   private val target: String,
                   private val rootFolder: String,
                   private val ACERoot: String,
                   private val mode: String = "nfold") {

  private val goal = kbase.getPredicate(target)
  private var nfolds = 5
  private val outputFolder = s"$rootFolder/" + target.replace("-", "_")
  private val rules = collection.mutable.Set[(String,String)]()

  // create directory for output files
  val dir = new File(outputFolder)
  dir.mkdir()

  def setFoldNumber(value: Int) = { nfolds = value }
  def getFoldNumber = { nfolds }

  def getOutputFolder = { outputFolder }
  def getTreeFileFolder = { getOutputFolder + "/tilde"}
  def getSettingsFile = { getOutputFolder + "/" + target.replace("-","_") + ".s" }
  def getKBFile = { getOutputFolder + "/" + target.replace("-","_") + ".kb" }
  def getRootFolder = { rootFolder }

  def getACERoot = { ACERoot }
  def getTargetPredicate = { target }

  private def addRule(head: String, body: String) = { rules += new Tuple2(head, body) }
  def getRules(posOnly: Boolean = true) = {
    if (posOnly) { rules.filter( !_._1.contains("!")) }
    else { rules }
  }

  private def createKnowledgeBase(filename: String) = {
    kbase.printToFileWithTarget(s"/$filename", target)
  }

  private def generateSettings(filename: String) = {
    val writer = new FileWriter(filename)

    // TILDE settings
    writer.write("load(key).\n")
    writer.write("talking(0).\n")
    writer.write("output_options([c45,prolog]).\n")
    writer.write("pruning(none).\n")
    writer.write("heuristic(gain).\n")
    writer.write("bias(warmode).\n")
    writer.write("tilde_mode(classify).\n")
    writer.write("classes([pos,neg]).\n")
    writer.write("minimal_cases(4).\n")
    writer.write("\n")

    //target predicate
    writer.write("predict(" + target.toLowerCase.replace('-','_') + "(+" + goal.getDomains.mkString(",+") + ",-class)).\n")
    writer.write("\n")

    //types
    writer.write("typed_language(yes).\n\n")
    //other predicates
    for (predicate <- kbase.getPredicateNames) {
      if (predicate != target) {
        val temp_predicate = kbase.getPredicate(predicate)
        writer.write("warmode(" + predicate.toLowerCase.replace('-','_') + "(+-" + temp_predicate.getDomains.mkString(",+-") + ")).\n")
        writer.write("type(" + predicate.toLowerCase.replace('-','_') + "(+-" + temp_predicate.getDomains.mkString(",+-") + ")).\n")
      }
    }

    writer.close()
  }

  private def generateInduceCommand = {
    if (mode == "nfold") { s"nfold(tilde,$nfolds)" }
    else { "induce(tilde)" }
  }

  def run() = {
    println(s"Learning for predicate $getTargetPredicate")
    generateSettings(getSettingsFile)
    createKnowledgeBase(getKBFile)
    val out = new FileWriter(getRootFolder + "/tilde.log")
    val err = new FileWriter(getRootFolder + "tilde.err")

    (Process(Seq("echo",generateInduceCommand), new File(getOutputFolder), "ACE_ILP_ROOT" -> getACERoot)
                  #| Process(Seq(getACERoot + "/bin/ace"), new File(getOutputFolder), "ACE_ILP_ROOT" -> getACERoot)
                ) ! ProcessLogger(line => out.write(line + "\n"), line => err.write(line + "\n"))
    out.close()
    err.close()
    extractRules()
  }

  private def extractRulesFromFile(filename: String) = {
    var parse = false

    Helper.readFile(filename).foreach( line => {
      if (line.startsWith("Equivalent prolog program")) {
        parse = true
      }
      else if (parse && line.contains(":-")) {
        val tmp = formatRule(line).split("<=>")
        addRule(tmp.head.trim, tmp(1).trim)
      }
    })
  }

  private def extractRules() = {
    if (mode == "nfold") {
      (1 to nfolds).foreach( fold => extractRulesFromFile(getTreeFileFolder + s"/$target.uB$fold") )
    }
    else {
      extractRulesFromFile(getTreeFileFolder + s"/$target.uB")
    }
  }

  /*
    Formats prolog rule back to original Alchemy syntax
   */
  private def formatRule(rule: String) = {
    val splitted = rule.toLowerCase.replace("[","").replace("]","").replace(", !.", "").split(":-")
    val body = splitted(1).split( """\),""").map(_.trim.capitalize)
    val head =  splitted.head.contains(",neg)") match {
      case true => "!" + splitted.head.replace(",neg", "").capitalize
      case false => splitted.head.replace(",pos", "").capitalize
    }

    var resultString = Helper.orderVariables(head.trim + " <=> " + body.mkString(" ^ "))
    var keepSwitching = true
    var varInd = 0

    // orderVariables replaces variables with numbers, this part puts the variables back
    while (keepSwitching) {
      if (resultString.contains(varInd.toString)) { resultString = resultString.replace(varInd.toString, ('a'.toInt + varInd).toChar.toString); varInd += 1 }
      else { keepSwitching = false }
    }

    resultString
  }

}
