package deepSRL.basics

/**
  * Created by seb on 16.12.15.
  */
class Domain(val name: String) {

  var elements = collection.mutable.Set[String]()

  def size = { elements.size }

  def hasElement(element: String) = { elements.contains(element) }

  def getElements = { elements }

  def getElementsAsList = { elements.map( List[String](_)) }

  def elementsNotIn(coll: Set[String]) = { elements.diff(coll) }

  def elementsNotInList(coll: Set[List[String]]) = { elements.map( List[String](_)).diff(coll) }

  def addElement(elem: String) = { elements += elem }

  def removeElement(elem: String) = { elements = elements - elem }

  def getName = { name }

}
