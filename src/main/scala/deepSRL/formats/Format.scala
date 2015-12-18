package deepSRL.formats

/**
  * Created by seb on 17.12.15.
  */
object Format {

  def tildeFormatFact(predicate: String, arguments: String) = {
    predicate.toLowerCase + "(" + arguments.toLowerCase + ")."
  }

  def tildeFormatDefinition(predicate: String, arguments: String) = {
    predicate.toLowerCase + "(" + arguments.split(",").map( "+-" + _.toLowerCase ).mkString(",") + ")"
  }

  def MLNFormatFact(predicate: String, arguments: String) = {
    predicate.capitalize + "(" + arguments.split(",").map( _.capitalize ).mkString(",") + ")"
  }

  def MLNFormatDefinition(predicate: String, arguments: String) = {
    predicate.capitalize + "(" + arguments.split(",").map( _.toLowerCase() ).mkString(",") + ")"
  }

}
