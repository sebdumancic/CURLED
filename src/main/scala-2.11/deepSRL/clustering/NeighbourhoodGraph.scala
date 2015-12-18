package deepSRL.clustering

import deepSRL.basics.{Predicate, KnowledgeBase}

/**
 * Implements neighbourhood graph functionality that is used to estimate indirect links measure within similarity measure
 *
 * implements a rooted graph, where a root is an object of interest
 *
 * Created by seb on 16.10.15.
 */
class NeighbourhoodGraph(val rootObject: String,
                         val domain: String,
                         val depth: Int,
                         val kBase: KnowledgeBase) {

  val root = new Node(rootObject, domain)
  val nodes = collection.mutable.Map[String, Node]() //collection of all connected objects for easier manipulation
  nodes(rootObject) = getRoot

  def getKnowledgeBase = {
    kBase
  }

  def getRootDomain = {
    domain
  }

  def getRoot = {
    root
  }

  def getMaxDepth = {
    depth
  }

  def addNode(node: Node) = {
    nodes(node.getEntity) = node
  }

  private def constructNodeEdges(node: Node) = {
    //includes only relationship into account here
    getKnowledgeBase.getPredicateNames.map(kBase.getPredicate).filter(_.arity > 1).filter(_.getDomains.contains(node.getDomain)).foreach(pred => {
      constructEdgeGivenPredicate(node, pred)
    })
  }

  private def constructEdgeGivenPredicate(node: Node, predicate: Predicate) = {

    //iterate over potential bindings
    predicate.getDomains.zipWithIndex.filter(_._1 == node.getDomain).foreach(binding => {
      //find children - true groundings
      predicate.getTrueGroundings.filter(_ (binding._2) == node.getEntity).foreach(ground => {
        ground.zipWithIndex.filterNot(_._1 == node.getEntity).foreach(child => {
          //filterNot - no recursive links allowed

          //if node does not exits, create it first
          if (!nodes.contains(child._1)) {
            addNode(new Node(child._1, predicate.getDomains(child._2)))
          }

          //connect the nodes
          node.addChild(nodes(child._1), predicate)
          nodes(child._1).addParent(node, predicate)
        })
      })
    })
  }

  def construct() = {
    var frontier = List[Node](getRoot)
    var currentDepth = 0
    var newFrontier = List[Node]()

    while (currentDepth <= getMaxDepth) {

      //extend current frontier
      frontier.foreach(cNode => {
        constructNodeEdges(cNode)
        newFrontier = newFrontier ++ cNode.getChildNodes
      })

      //update frontiers (newFrontier becomes current frontier, empty newFrontier)
      //frontier = List[Node]()
      frontier = newFrontier.map(x => x)
      newFrontier = List[Node]()

      currentDepth += 1
    }
  }

  /*
    collects the information about types encountered on each level of graph

    :returns: a Map[Int,Map[String1, List[String]]] (Int -> Level starting with 0, String1 -> domains, List => list of found objects)
   */
  def collectTypeInformation() = {
    construct()
    val resultSummary = collection.mutable.Map[Int,collection.mutable.Map[String,List[String]]]()
    var currentLevel = 0
    var frontier = List[Node](getRoot)
    var newFrontier = List[Node]()

    while (currentLevel <= getMaxDepth) {

      if (!resultSummary.contains(currentLevel)) {
        resultSummary(currentLevel) = collection.mutable.Map[String, List[String]]()
      }

      //expand all nodes in a frontier, store the information
      frontier.foreach(cNode => {
        cNode.getChildNodes.foreach(child => {
          newFrontier = newFrontier :+ child

          if (!resultSummary(currentLevel).contains(child.getDomain)) {
            resultSummary(currentLevel)(child.getDomain) = List[String]()
          }
          resultSummary(currentLevel)(child.getDomain) = resultSummary(currentLevel)(child.getDomain) :+ child.getEntity
        })
      })
      frontier = newFrontier.map(x => x)
      newFrontier = List[Node]()
      currentLevel += 1
    }
    resultSummary
  }

  def collectTypeInformation(level: Int): Map[String, List[String]] = {
    val allInformation = collectTypeInformation(); allInformation(level).toMap
  }

  def getRootPredicates = {
    getRoot.getChildEdges.map(_.getPredicate)
  }

  override def toString = {
    getRoot.asString("")
  }
}

class Node(val entity: String,
           val domain: String) {

  var parents = Set[Edge]()
  var children = Set[Edge]()

  def getDomain = {
    domain
  }

  def getEntity = {
    entity
  }

  def addParent(parent: Node, predicate: Predicate) = {
    if (parent.getEntity != getEntity || parent.getDomain != getDomain) {
      parents = parents + new Edge(parent, this, predicate)
    }
  }

  def getParentNodes = {
    parents.map(_.getParent)
  }

  def getParentEdges = {
    parents
  }

  def addChild(child: Node, predicate: Predicate) = {
    if ((child.getEntity != getEntity || child.getDomain != getDomain) && !getParentEdges.map(_.getParent).contains(child)) {
      children = children + new Edge(this, child, predicate)
    }
  } //takes care of cycle edges

  def getChildEdges = {
    children
  }

  def getChildRelationships = {
    children.map(_.getPredicate)
  }

  def getChildNodes = {
    children.map(_.getChild)
  }

  def getTypedChildren(domainType: String) = {
    children.filter(x => {
      x.getChild.getDomain == domainType
    })
  }

  def getPredicateChildren(predicate: Predicate) = {
    children.filter(_.getPredicate == predicate).map(_.getChild)
  }

  def asString(prefix: String): String = {
    s"$getEntity[$getDomain]\n" +
      prefix + "|---" + getChildNodes.map(_.asString(s"$prefix|   ")).mkString(s"\n$prefix|---")
  }

  override def toString = {
    s"$getEntity[$getDomain]"
  }

  override def hashCode = {
    toString.hashCode
  }

  override def equals(other: Any) = other match {
    case other: Node => hashCode == other.hashCode
    case _ => false
  }

}

class Edge(val parentNode: Node,
           val childNode: Node,
           val relationshipPredicate: Predicate) {

  def getChild = {
    childNode
  }

  def getParent = {
    parentNode
  }

  def getPredicate = {
    relationshipPredicate
  }

  override def toString = {
    s"$getParent --[$getPredicate]--> $getChild"
  }

  override def equals(other: Any) = other match {
    case other: Edge => hashCode == other.hashCode
    case _ => false
  }

  override def hashCode = {
    toString.hashCode
  }
}