package learners

import logicalPrimitives.AbstractDefinition

/** Abstract learner aby learner should extend
  * Created by seb on 08.02.16.
  */
abstract class AbstractLearner {

  /** Fits the models to the knowledge base */
  def fitModel(): Unit

  /** Extracts the definitions of new predicates */
  def getDefinitions: Set[ _ <: AbstractDefinition]

}
