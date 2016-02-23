package logicalPrimitives

/** Container for a definition of a cluster
  *
  * @constructor create a definition object give na string representation of a prolog rule
  * @param stringRep prolog rule as string
  * Created by seb on 23.02.16.
  */
abstract class PrologDefinition(protected val stringRep: String) {

  protected var absoluteCoverage: Int = 0
  protected var relativeCoverage: Double = 0.0

  protected var head: String = null
  protected var body: List[String] = null

  protected val predicateRegex = """(.*)\((.*)\)""".r

  /** Set the absolute coverage value
    *
    * @param value value to set
    * */
  def setAbsCoverage(value: Int) = {
    absoluteCoverage = value
  }

  /** Set the relative coverage
    *
    * @param value value to set
    * */
  def setRelCoverage(value: Double) = {
    relativeCoverage = value
  }

  /** Adds a head predicate
    *
    * @param headPred head predicate
    * */
  protected def addHead(headPred: String) = {
    head = headPred
  }

  /** Adds a head predicate and cleans the indication of positive example [e.g., cluster(element,[pos]) -> cluster(element)]
    * */
  protected def addHeadAndClean(headPred: String) = {
    head = headPred.replace(",[pos])", ")")
  }

  /** Adds a predicate to body
    *
    * @param definition predicate to be added to the body
    * */
  protected def addBodyPred(definition: String) = {
    body = body :+ definition
  }

  /** Parses the string*/
  protected def parseString() = {
    val splitted = stringRep.split(":-")
    addHeadAndClean(splitted.head)

    splitted(1).trim.split("),").filter( _.trim != "!.").map( _ + ")").foreach( bp => addBodyPred(bp))
  }

  /** Returns the rule in Prolog format */
  def asProlog = {
    s"$head :- ${body.mkString(",")}."
  }

  def asMLN = {
    s"${body.map(prologToMLN).mkString(" ^ ")} <=> ${prologToMLN(head)}"
  }

  /** Re-format prolog representation to MLN
    *
    * @param predicate predicate in prolog syntax: String
    * */
  protected def prologToMLN(predicate: String) = {
    val predicateRegex(pred, args) = predicate
    s"${pred.capitalize}(${args.split(",").map(x => if (x.length > 1) x.capitalize else x.toLowerCase).mkString(",")})"
  }

  /** Returns the absolute coverage of a rule*/
  def getAbsCoverage = {
    absoluteCoverage
  }

  /** Returns the relative coverage of a rule*/
  def getRelCoverage = {
    relativeCoverage
  }

}
