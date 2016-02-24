package logicalPrimitives

/**
  * Created by seb on 24.02.16.
  */
abstract class AbstractDefinition(protected val stringRep: String) {

  protected def parseString(): Unit

  def asProlog: String

  def asMLN: String

  override def toString = {
    asMLN
  }

  override def hashCode = {
    toString.hashCode
  }

  override def equals(that: Any) = that match {
    case that: AbstractDefinition => this.toString == that.toString
    case _ => false
  }
}
