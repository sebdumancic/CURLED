package representationLearning

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import learners.ilp.ace.{TildeInduce, TildeNFold}
import org.clapper.argot.ArgotParser
import relationalClustering.bagComparison.bagCombination.{IntersectionCombination, UnionCombination}
import relationalClustering.bagComparison.{ChiSquaredDistance, MaximumSimilarity, MinimumSimilarity, UnionBagSimilarity}
import relationalClustering.clustering.evaluation.{AverageIntraClusterSimilarity, SilhouetteScore}
import relationalClustering.clustering.{Hierarchical, Spectral}
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.utils.{Helper, PredicateDeclarations, Settings}
import representationLearning.clusterComparison.OverlapWithARI
import representationLearning.clusterSelection.{IncreaseSaturationCut, ModelBasedSelection, PredefinedNumber}
import representationLearning.layer.{AdaptiveSelectionLayer, DefinitionBasedLayer}

/**
  * CLI implementing the functionality of learning new representation with
  * Created by seb on 09.02.16.
  */
object LearnNewRepresentation {

  //parser specification
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser("LearnNewRepresentation.jar", preUsage = Some("Version 2.11"))
  val dbs = parser.option[String](List("db"), "knowledgeBase", "comma-separated list of database(s) with data to cluster")
  val head = parser.option[String](List("domain"), "domain definition", "header for the knowledge base(s); specification of logical predicates")
  val declarationFile = parser.option[String](List("declarations"), "file path", "file containing declarations of predicates")
  val depth = parser.option[Int](List("depth"), "n", "depth of the neighbourhood graph")
  val rootFolder = parser.option[String](List("root"), "filePath", "folder to place files in")
  val weights = parser.option[String](List("weights"), "Array[Double]", "semi-colon separated list of parameter sets; each set is a comma-separated list of 5 doubles [attributes,attribute distribution,connections,vertex neighbourhood, edge distribution]")
  val algorithm = parser.option[String](List("algorithm"), "[Spectral|Hierarchical]", "algorithm to perform clustering")
  val similarity = parser.option[String](List("similarity"), "[RCNT|RCNTv2|HS|RIBL|HSAG]", "similarity measure")
  val bag = parser.option[String](List("bagSimilarity"), "[chiSquared|maximum|minimum|union]", "bag similarity measure")
  val bagCombination = parser.option[String](List("bagCombination"), "[union|intersection]", "bag combination method")
  val linkage = parser.option[String](List("linkage"), "[average|complete|ward]", "linkage for hierarchical clustering")
  val useLocalRepository = parser.flag[Boolean](List("localRepo"), "should NodeRepository be constructed locally for each NeighbourhoodGraph, or one globally shared")
  val query = parser.option[String](List("query"), "comma-separated list", "list of domains to cluster; if not set, all domains are clustered")
  val maxNumberOfClusters = parser.option[Int](List("maxClusters"), "n", "maximal number of clusters to create, per domain")
  val selectionMethod = parser.option[String](List("selection"), "predefined|model|saturation|definition", "how to select the number of clusters per domain")
  val clusteringValidation = parser.option[String](List("clusterValidation"), "silhouette|intraClusterSimilarity", "cluster validation method")
  val tradeOffFactor = parser.option[Double](List("saturationTradeOff"), "Double", "saturation trade off parameter: sim(i) >= w * sim(i+1) [default: 0.9]")
  val outputName = parser.option[String](List("output"), "string", "name of the file to save new layer [default:newLayer.*]")
  val k = parser.option[Int](List("k"), "n", "desired number of clusters in 'predefined' selection method is used")
  val kPerDomain = parser.option[String](List("kDomain"), "comma-separated list of domain:numClusters", "number of clusters per domain")
  val clusterEdges = parser.flag[Boolean](List("clusterHyperedges"), "should hyperedges be clusters as well (between the specified domains)")
  val extractDefinitions = parser.flag[Boolean](List("extractDefs"), "extract the definitions of new predicates")
  val defLearner = parser.option[String](List("definitionLearner"), "TildeInduce|TildeNFold", "which learner to use for definition extraction")
  val tildeHeuristic = parser.option[String](List("tildeHeuristic"), "gain|gainratio", "heuristics to use with TILDE [default: gain]")
  val tildeMinCases = parser.option[Int](List("tildeMinCases"), "n", "minimal number of cases [default: 4]")
  val nFold = parser.option[Int](List("numFolds"), "n", "number of folds for N-fold validation [default: 10]")
  val overlapThreshold = parser.option[Double](List("overlapThreshold"), "Double [0.3]", "if overlap measure smaller than this threshold, a cluster is accepted as a new predicate")
  val featureFormat = parser.flag[Boolean](List("asFeature"), "should feature representation be used to store new representation")
  val minimalCoverage = parser.option[Double](List("minimalCoverage"), "double", "minimal coverage for a definition to be considered as large-coverage (percentage)")
  val newFacts = parser.option[String](List("newKB"), "filename", "a knowledge base to be mapped to the latent representation")
  val latentOutput = parser.option[String](List("latentOutput"), "filename", "file to save a transformed [newKB] representation")


  def printParameters() = {
    println("CLUSTERING WITH THE FOLLOWING PARAMETERS")
    println(s"---- databases ${dbs.value.get.split(",").toList}")
    println(s"---- depth: ${depth.value.getOrElse(0)}")
    println(s"---- parameter set: ${weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2")}")
    println(s"---- clustering algorithm: ${algorithm.value.getOrElse("Spectral")}")
    println(s"---- similarity measure: ${similarity.value.getOrElse("RCNT")}")
    println(s"---- bag similarity measure: ${bag.value.getOrElse("chiSquared")}")
    println(s"---- bag combination method: ${bagCombination.value.getOrElse("union")}")
    println(s"---- linkage (for Hierarchical clustering): ${linkage.value.getOrElse("average")}")
    println(s"---- using local directory: ${useLocalRepository.value.getOrElse(false)}")
    println(s"---- maximal number of clusters: ${maxNumberOfClusters.value.getOrElse(10)}")
    println(s"---- saving everything with name: ${outputName.value.getOrElse("newLayer")}")
    println()
    println(s"---- query: ${query.value.get}")
    println(s"---- clustering selection method: ${selectionMethod.value.getOrElse("saturation")}")
    println(s"---- clustering validation method: ${clusteringValidation.value.getOrElse("intraClusterSimilarity")}")
    println(s"---- saturation selection trade-off factor: ${tradeOffFactor.value.getOrElse(0.9)}")
    println(s"---- clustering edges: ${clusterEdges.value.getOrElse(false)}")
    println(s"---- k: ${k.value.getOrElse(0)}")
    println(s"---- k per domains: ${kPerDomain.value.getOrElse("")}")
    println()
    if (extractDefinitions.value.getOrElse(false)) {
      println(s"---- learning definitions of new predicates: true")
      println(s"---- extracting definitions with: ${defLearner.value.getOrElse("TildeInduce")}")
      defLearner.value.getOrElse("TildeInduce") match {
        case "TildeInduce" =>
          println(s"---- tilde heuristics: ${tildeHeuristic.value.getOrElse("gain")}")
          println(s"---- tilde minimal number of cases: ${tildeMinCases.value.getOrElse(1)}")
        case "TildeNFold" =>
          println(s"---- tilde heuristics: ${tildeHeuristic.value.getOrElse("gain")}")
          println(s"---- tilde minimal number of cases: ${tildeMinCases.value.getOrElse(1)}")
          println(s"---- number of folds: ${nFold.value.getOrElse(10)}")
      }
    }
    println()
    println()

  }

  def main(args: Array[String]) {
    parser.parse(args)
    require(head.hasValue, "no header specified")
    require(dbs.hasValue, "no databases specified")
    require(query.hasValue, "query not specified")
    require(declarationFile.hasValue, "declarations of the predicates not provided")

    printParameters()

    val predicateDeclarations = new PredicateDeclarations(declarationFile.value.get)
    val KnowledgeBase = new KnowledgeBase(dbs.value.get.split(","), Helper.readFile(head.value.get).mkString("\n"), predicateDeclarations)

    // domains to cluster
    val domainsToCluster = query.value.isEmpty match {
      case true => KnowledgeBase.getAllDomains.keys.toList
      case false => query.value.get.split(",").toList
    }

    // number of clusters per domain
    val clusterPerDomain = kPerDomain.value.getOrElse("").length > 1 match {
      case true =>
        val doms = collection.mutable.Map[String, Int]()
        kPerDomain.value.get.split(",").foreach( elem => {val tup = elem.split(":"); doms(tup.head) = tup(1).toInt })
        domainsToCluster.map( doms(_) )
      case false => domainsToCluster.map( x => k.value.getOrElse(0) ) // if not specified the number of clusters per domain, it is either specified that all have the same value, or other procedure is used
    }



    // similarity measure to use

    val bagComparison = bag.value.getOrElse("chiSquared") match {
      case "chiSquared" => new ChiSquaredDistance()
      case "minimum" => new MinimumSimilarity()
      case "maximum" => new MaximumSimilarity()
      case "union" => new UnionBagSimilarity() //new Unionsimilarity()
    }

    val bagCombinationMethod = bagCombination.value.getOrElse("union") match {
      case "union" => new UnionCombination()
      case "intersection" => new IntersectionCombination()
    }

    // clustering algorithm
    val clustering = algorithm.value.getOrElse("Spectral") match {
      case "Spectral" =>
        new Spectral(rootFolder.value.getOrElse("./tmp"))

      case "Hierarchical" =>
        new Hierarchical(linkage.value.getOrElse("average"), rootFolder.value.getOrElse("./tmp"))
    }

    //cluster validation
    val clusterValidationMethod = clusteringValidation.value.getOrElse("intraClusterSimilarity") match {
      case "silhouette" => new SilhouetteScore(rootFolder.value.getOrElse("./tmp"))
      case "intraClusterSimilarity" => new AverageIntraClusterSimilarity()
    }

    val clusterSelector = selectionMethod.value.getOrElse("saturation") match {
      case "predefined" => new PredefinedNumber(clusterPerDomain.head)
      case "model" => new ModelBasedSelection(clusterValidationMethod)
      case "saturation" => new IncreaseSaturationCut(clusterValidationMethod, tradeOffFactor.value.getOrElse(0.9))
      case _ =>  new PredefinedNumber(clusterPerDomain.head)
    }


    val clusterOverlap = new OverlapWithARI(rootFolder.value.getOrElse("./tmp"))

    val parameterSets = weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(":").toList.map( par => par.split(",").toList.map( _.toDouble ))

    try {

      val firstLayer = selectionMethod.value.getOrElse("saturation") match {
        case "definition" =>

          val learnerDef = Map[String,String]("algorithm" -> defLearner.value.getOrElse("TildeInduce"),
                                              "heuristic" -> tildeHeuristic.value.getOrElse("gain"),
                                              "minCases" -> tildeMinCases.value.getOrElse(1).toString,
                                              "ACE_ROOT" -> sys.env.getOrElse("ACE_ILP_ROOT", "/home/seba/Software/ACE-ilProlog-1.2.20/linux"))

          new DefinitionBasedLayer(KnowledgeBase,
                                  domainsToCluster,
                                  depth.value.getOrElse(0),
                                  maxNumberOfClusters.value.getOrElse(10),
                                  similarity.value.getOrElse("RCNT"),
                                  bagComparison,
                                  bagCombinationMethod,
                                  clustering,
                                  minimalCoverage.value.getOrElse(0.1),
                                  learnerDef,
                                  parameterSets,
                                  clusterOverlap,
                                  overlapThreshold.value.getOrElse(0.3),
                                  clusterEdges.value.getOrElse(false),
                                  outputName.value.getOrElse("newLayer"),
                                  rootFolder.value.getOrElse("./tmp"),
                                  featureFormat.value.getOrElse(false))
        case _ => new AdaptiveSelectionLayer(rootFolder.value.getOrElse("./tmp"),
                                            outputName.value.getOrElse("newLayer"),
                                            KnowledgeBase,
                                            domainsToCluster,
                                            depth.value.getOrElse(0),
                                            bagComparison,
                                            bagCombinationMethod,
                                            similarity.value.getOrElse("RCNT"),
                                            clustering,
                                            clusterSelector,
                                            clusterOverlap,
                                            overlapThreshold.value.getOrElse(0.3),
                                            maxNumberOfClusters.value.getOrElse(10),
                                            parameterSets,
                                            clusterEdges.value.getOrElse(false),
                                            featureFormat.value.getOrElse(false)
        )
      }

      val newRep = firstLayer.build()

      val (headerH, declH, kbH) = newRep.write(outputName.value.getOrElse("newLayer"), rootFolder.value.getOrElse("./tmp"))
      val newHeaderFile = new File(headerH)
      val newDeclarationFile = new File(declH)
      val newKBFile = new File(kbH)


      //extract definitions of discovered predicates
      if (extractDefinitions.value.getOrElse(false)) {
        val latentPredicateDeclarations = new PredicateDeclarations(newDeclarationFile.getAbsolutePath)
        val latentKB = new KnowledgeBase(Seq(newKBFile.getAbsolutePath),
          Helper.readFile(newHeaderFile.getAbsolutePath).mkString("\n"),
                                         latentPredicateDeclarations)

        println("\n\n\n FOUND PREDICATES")
        latentKB.getPredicateNames.map(latentKB.getPredicate).foreach( pred => {

          val learner = defLearner.value.getOrElse("TildeInduce") match {
            case "TildeInduce" => new TildeInduce(rootFolder.value.getOrElse("./tmp"), KnowledgeBase, latentKB, pred.getName, pred.getRole == Settings.ROLE_HYPEREDGE,
                                                  sys.env.getOrElse("ACE_ILP_ROOT", "/home/seba/Software/ACE-ilProlog-1.2.20/linux"), tildeHeuristic.value.getOrElse("gain"),
                                                  tildeMinCases.value.getOrElse(1))
            case "TildeNFold" => new TildeNFold(rootFolder.value.getOrElse("./tmp"), KnowledgeBase, latentKB, pred.getName, pred.getRole == Settings.ROLE_HYPEREDGE,
                                                sys.env.getOrElse("ACE_ILP_ROOT", "/home/seba/Software/ACE-ilProlog-1.2.20/linux"), nFold.value.getOrElse(10),
                                                tildeHeuristic.value.getOrElse("gain"), tildeMinCases.value.getOrElse(1))
          }

          learner.fitModel()

          println("*"*10)
          learner.getDefinitions.foreach(pd => println(s"  ${pd.getAbsCoverage}(${pd.getRelCoverage}) ::  $pd"))
          println("*"*10)

        })
      }

      if (newFacts.value.getOrElse("Nil") != "Nil") {
        val latentKB = new KnowledgeBase(List(newFacts.value.get), Helper.readFile(head.value.get).mkString("\n"), predicateDeclarations)
        val latentFacts = newRep.mapNewFacts(latentKB, linkage.value.getOrElse("average"))
        val LKBOutput = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${latentOutput.value.getOrElse(s"latent_representation.db")}"))
        LKBOutput.write(latentFacts.toList.sorted.mkString(sys.props("line.separator")))
        LKBOutput.close()
      }
    }
    catch {
      case e: Exception => println(s"ERROR: ${e.getMessage}\n \t ${e.getStackTrace}")
        val writeError = new PrintWriter(s"${rootFolder.value.getOrElse("./tmp")}/error.log")
        e.printStackTrace(writeError)
        writeError.close()
    }
  }
}
