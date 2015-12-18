package deepSRL.basics

/**
 *  This class implements predicate functionality for finding candidates to combine.
 *  It stores the basic information about the predicate, as well as true groundings
 *
 * Created by seb on 08.05.15.
 */
class Predicate(
               var name: String,
               val domains: List[Domain]
                 ) {

  private val trueGroundings: collection.mutable.Set[List[String]] = collection.mutable.Set[List[String]]()

  def getTrueGroundings: collection.mutable.Set[List[String]] = { trueGroundings }

  def setTrueGrounding(ground: List[String]) = {
    require( ground.size == arity, "Wrong number of arguments")
    trueGroundings += ground

    //add each elements to its domain
    ground.zipWithIndex.foreach( elem => domains(elem._2).addElement(elem._1))
  }

  def removeTrueGrounding(ground: List[String]) = {
    require( ground.size == arity, "Wrong number of arguments")
    trueGroundings -= ground
  }

  def arity: Int = { domains.size }

  def getName: String = { name }

  def getDomains: List[String] = { domains.map( _.getName ) }

  def getDomainObjects = { domains }
  
  def isTrue(grounding: List[String]) = {
    trueGroundings.contains(grounding)
  }

  override def toString = getName + "(" + getDomains.mkString(",") + ")"

  def setName(newName: String) = { name = newName}
}
