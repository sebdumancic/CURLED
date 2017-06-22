package parameters

import oscar.cp._
import relationalClustering.aggregators.AbstractAggregator
import relationalClustering.bags.bagComparison.AbstractBagComparison
import relationalClustering.clustering.evaluation.semisupervised.ConstraintsContainer
import relationalClustering.neighbourhood.NeighbourhoodTree
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.similarity.SimilarityNeighbourhoodTrees

/**
  * Created by seb on 15.06.17.
  */
class IterativeCoverage(protected val knowledgeBase: KnowledgeBase,
                        protected val domain: String,
                        protected val constraints: ConstraintsContainer,
                        protected val depth: Int,
                        protected val bagComparison: AbstractBagComparison,
                        protected val aggregators: List[AbstractAggregator],
                        protected val folder: String = "./tmp") extends CPModel {

  protected val mustLinks: Set[(NeighbourhoodTree, NeighbourhoodTree)] = constraints.getMustLink
  protected val cannotLinks: Set[(NeighbourhoodTree, NeighbourhoodTree)] = constraints.getCannotLink

  protected val firstSimilarityMeasure = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(1.0,0.0,0.0,0.0,0.0), bagComparison, null, aggregators)
  protected val secondSimilarityMeasure = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(0.0,1.0,0.0,0.0,0.0), bagComparison, null, aggregators)
  protected val thirdSimilarityMeasure = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(0.0,0.0,1.0,0.0,0.0), bagComparison, null, aggregators)
  protected val fourthSimilarityMeasure = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(0.0,0.0,0.0,1.0,0.0), bagComparison, null, aggregators)
  protected val fifthSimilarityMeasure = new SimilarityNeighbourhoodTrees(knowledgeBase, depth, List(0.0,0.0,0.0,0.0,1.0), bagComparison, null, aggregators)

  firstSimilarityMeasure.getObjectSimilaritySave(List(domain), folder)
  secondSimilarityMeasure.getObjectSimilaritySave(List(domain), folder)
  thirdSimilarityMeasure.getObjectSimilaritySave(List(domain), folder)
  fourthSimilarityMeasure.getObjectSimilaritySave(List(domain), folder)
  fifthSimilarityMeasure.getObjectSimilaritySave(List(domain), folder)



  def findParameters(): List[List[Double]] = {
    val constraints = mustLinks.foldLeft(Set[Set[List[Int]]]())((acc, ml) => acc ++ coverMustLink(ml, cannotLinks)).filter(_.nonEmpty)

    selectSimilarityInterpretations(constraints)
  }

  protected def coverMustLink(mustLink: (NeighbourhoodTree, NeighbourhoodTree), cannotLinks: Set[(NeighbourhoodTree, NeighbourhoodTree)]): Set[Set[List[Int]]] = {
    cannotLinks.foldLeft(Set[Set[List[Int]]]())((acc, cl) => acc + mustVsCannotLink(mustLink, cl))
  }

  protected def mustVsCannotLink(mustLink: (NeighbourhoodTree, NeighbourhoodTree), cannotLink: (NeighbourhoodTree, NeighbourhoodTree)): Set[List[Int]] = {
    val mlComponents = getComponents(mustLink).map(e => (e*100).toInt)
    val clComponents = getComponents(cannotLink).map(e => (e*100).toInt)

    findAllSimilarityInterpretations(mlComponents, clComponents)
  }

  protected def getComponents(constraint: (NeighbourhoodTree, NeighbourhoodTree)): List[Double] = {
    List(firstSimilarityMeasure.pairObjectSimilarity(constraint._1, constraint._2),
         secondSimilarityMeasure.pairObjectSimilarity(constraint._1, constraint._2),
         thirdSimilarityMeasure.pairObjectSimilarity(constraint._1, constraint._2),
         fourthSimilarityMeasure.pairObjectSimilarity(constraint._1, constraint._2),
         fifthSimilarityMeasure.pairObjectSimilarity(constraint._1, constraint._2)
    )
  }

  protected def findAllSimilarityInterpretations(MLCoefficients: List[Int], CLCoefficients: List[Int]): Set[List[Int]] = {
    val solutions = collection.mutable.Set[List[Int]]()
    val w1 = CPIntVar(0 to 1)
    val w2 = CPIntVar(0 to 1)
    val w3 = CPIntVar(0 to 1)
    val w4 = CPIntVar(0 to 1)
    val w5 = CPIntVar(0 to 1)

    val varArray = Array(w1, w2, w3, w4, w5)

    add(w1 * MLCoefficients.head + w2 * MLCoefficients(1) + w3 * MLCoefficients(2) + w4 * MLCoefficients(3) + w5 * MLCoefficients(4) >
      w1 * CLCoefficients.head + w2 * CLCoefficients(1) + w3 * CLCoefficients(2) + w4 * CLCoefficients(3) + w5 * CLCoefficients(4))

    add(sum(varArray) > 0)

    search {
      binaryFirstFail(varArray)
    } onSolution {
      if (varArray.forall(_.isBound)) {
        solutions += List(w1.value, w2.value, w3.value, w4.value, w5.value)
      }
    }

    start()

    solutions.toSet
  }

  protected def selectSimilarityInterpretations(conditions: Set[Set[List[Int]]]): List[List[Double]] = {
    val mapping = mapInterpretationsToVars(conditions)
    val solutions = collection.mutable.Set[List[Int]]()

    conditions.foreach(cond => add( or(cond.map(elem => mapping(elem))) ) )
    minimize(sum(mapping.values))

    search {
      binaryFirstFail(mapping.values.toSeq)
    } onSolution {
      println(s"Solution found: ${mapping.filter(_._2.value > 0)}")
      solutions ++= mapping.filter(_._2.value > 0).keys
    }

    start()

    solutions.toList.map(elem => {val denominator = elem.sum.toDouble; elem.map(_/denominator)})
  }

  protected def mapInterpretationsToVars(conditions: Set[Set[List[Int]]]): Map[List[Int], CPBoolVar] = {
    conditions.flatten.map(si => (si, CPBoolVar())).toMap
  }


  protected def inequalitySatisfied(MLCoefficients: List[Double], CLCoefficients: List[Double], weights: List[Int]): Boolean = {
    MLCoefficients.zip(weights).filter(_._2 > 0).map(_._1).sum > CLCoefficients.zip(weights).filter(_._2 > 0).map(_._1).sum
  }

}
