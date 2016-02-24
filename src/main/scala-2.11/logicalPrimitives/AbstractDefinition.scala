package logicalPrimitives

/**
  * Created by seb on 24.02.16.
  */
abstract class AbstractDefinition(protected val stringRep: String) {

  protected def parseString(): Unit

  def asProlog: String

  def asMLN: String
}
