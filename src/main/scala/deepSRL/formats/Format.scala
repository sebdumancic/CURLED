package deepSRL.formats

/**
  * Created by seb on 17.12.15.
  */
object Format {

  def tildeFormat(predicate: String, arguments: String) = {
    predicate.toLowerCase + "(" + arguments.toLowerCase + ")."
  }

  def MLNFormatFact(predicate: String, arguments: String) = {
    predicate.capitalize + "(" + arguments.split(",").map( _.capitalize ).mkString(",") + ")"
  }

  def MLNFormatDefinition(predicate: String, arguments: String) = {
    predicate.capitalize + "(" + arguments.split(",").map( _.toLowerCase() ).mkString(",") + ")"
  }

}
