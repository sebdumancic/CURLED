package deepSRL.formats

/**
  * Created by seb on 17.12.15.
  */
object Format {

  def tildeFormat(predicate: String, arguments: String) = {
    predicate.toLowerCase + "(" + arguments.toLowerCase + ")."
  }

  def MLNFormat(predicate: String, arguments: String) = {
    predicate.capitalize + "(" + arguments.split(",").map( _.capitalize ).mkString(",") + ")"
  }

}
