package learners.ilp.ace

import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 17.02.16.
  */
class TildeInduce(override protected val rootFolder: String,
                  override protected val knowledgeBase: KnowledgeBase,
                  override protected val latentKnowledgeBase: KnowledgeBase,
                  override protected val targetPredicate: String,
                  override protected val isRelation: Boolean,
                  override protected val aceRootFolder: String,
                  protected val heuristic: String = "gain",
                  protected val minCases: Int = 4) extends AbstractACE(rootFolder, knowledgeBase, latentKnowledgeBase, targetPredicate, isRelation, aceRootFolder) {

  /** Returns the minimal number of examples to cover*/
  def getMinCases = {
    minCases
  }

  /** Returns the heuristic to use*/
  def getHeuristic = {
    heuristic
  }

  def runCommand = {
    "induce(tilde)"
  }

  def getAdditionalSettings = {
    s"""
      |heuristic($getHeuristic).
      |pruning(none).
      |tilde_mode(classify).
      |minimal_cases($getMinCases).
    """.stripMargin
  }

  protected def getFilesWithRules = {
    List(s"tilde/${getTargetPredicateName.toLowerCase}.out")
  }

}
