package deepSRL.utils

import java.io.FileWriter

import scala.io.Source

/**
 *
 * A helper object containing small functions that does not belong to any specific objects
 * Created by seb on 29.06.15.
 */
object Helper {

  def bool2int(value: Boolean): Int = {
    if (value) 1 else 0
  }

  def orderVariables(inputFormula: String) = {
    val predicateRegex = """([A-Za-z_!]+\(.*?\))""".r
    val foundPredicates = (predicateRegex findAllIn inputFormula).toList
    var variables = List[String]()

    foundPredicates.foreach( predicate => { predicate.replace(")", "").split("""\(""")(1).split(",").foreach( variable => { if (!variables.contains(variable)) {variables = variables :+ variable.trim} }) })

    var changeVariables = false
    var resultString = List[Char]()

    for ( char <- inputFormula) {
      if (char == ',') { resultString = resultString :+ ','}
      else if (char == '(') { changeVariables = true; resultString = resultString :+ '('}
      else if (char == ')') { changeVariables = false; resultString = resultString :+ ')'}
      else if (changeVariables) { resultString = resultString :+ ('0'.toInt + variables.indexOf(char.toString)).toChar}

      else { resultString = resultString :+ char }
    }
    resultString.mkString
  }

  def checkForDuplicates(item: List[String]) = {
    val topElement = item.sortWith( (x,y) => { item.count(_ == x) > item.count(_ == y) } ).head
    item.count( _ == topElement) > 1
  }

  def readFile(filename: String) = {
    val fileSource = Source.fromFile(filename)
    val lines = fileSource.getLines().mkString("\n")
    fileSource.close()
    lines.split("\n")
  }

}
