package learners.ilp

import relationalClustering.representation.domain.KnowledgeBase

/**
  * Created by seb on 24.02.16.
  */
class TildeNFold(override protected val rootFolder: String,
                 override protected val knowledgeBase: KnowledgeBase,
                 override protected val latentKnowledgeBase: KnowledgeBase,
                 override protected val targetPredicate: String,
                 override protected val isRelation: Boolean,
                 override protected val aceRootFolder: String,
                 protected val numFolds: Int,
                 override protected val heuristic: String = "gain",
                 override protected val minCases: Int = 4) extends TildeInduce(rootFolder, knowledgeBase, latentKnowledgeBase, targetPredicate, isRelation, aceRootFolder, heuristic, minCases){

  /** Returns the number of folds*/
  protected def getNumberOfFolds = {
    numFolds
  }

  override def runCommand = {
    s"nfold(tilde,$getNumberOfFolds)"
  }

  override def getFilesWithRules = {
    (1 to getNumberOfFolds).toList.map( fold => s"tilde/${getTargetPredicateName.toLowerCase}.uB$fold")
  }

}
