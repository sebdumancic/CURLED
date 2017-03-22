package representationLearning

import java.io.{File, PrintWriter}

import org.clapper.argot.ArgotParser
import relationalClustering.aggregators._
import relationalClustering.bagComparison.bagCombination.{IntersectionCombination, UnionCombination}
import relationalClustering.bagComparison.{ChiSquaredDistance, MaximumSimilarity, MinimumSimilarity, UnionBagSimilarity}
import relationalClustering.clustering.algo.{Hierarchical, Spectral}
import relationalClustering.clustering.evaluation.{AverageIntraClusterSimilarity, SilhouetteScore}
import relationalClustering.representation.domain.KnowledgeBase
import relationalClustering.utils.{Helper, PredicateDeclarations}
import representationLearning.clusterComparison.OverlapWithARI
import representationLearning.clusterSelection.{IncreaseSaturationCut, ModelBasedSelection, PredefinedNumber}
import representationLearning.layer.builder.LayerBuilder
import representationLearning.layer.{AdaptiveSelectionLayer, MaxLayer}

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
  val tradeOffFactor = parser.option[String](List("saturationTradeOff"), "comma-separated list (per layer)", "saturation trade off parameter: sim(i) >= w * sim(i+1) [default: 0.9]")
  val outputName = parser.option[String](List("output"), "string", "name of the file to save new layer [default:newLayer.*]")
  val k = parser.option[Int](List("k"), "n", "desired number of clusters in 'predefined' selection method is used")
  val kPerDomain = parser.option[String](List("kDomain"), "comma-separated list of domain:numClusters", "number of clusters per domain")
  val clusterEdges = parser.flag[Boolean](List("clusterHyperedges"), "should hyperedges be clusters as well (between the specified domains)")
  val extractDefinitions = parser.flag[Boolean](List("extractDefs"), "extract the definitions of new predicates")
  val definitionSupport = parser.option[Double](List("definitionSupport"), "double [0.9]", "minimal support for definition mining (% of objects in a cluster)")
  val definitionDeviance = parser.option[Double](List("definitionDeviance"), "double [0.2]", "maximal deviance for definition mining (% of the mean value)")
  val overlapThreshold = parser.option[Double](List("overlapThreshold"), "Double [0.3]", "if overlap measure smaller than this threshold, a cluster is accepted as a new predicate")
  val featureFormat = parser.flag[Boolean](List("asFeature"), "should feature representation be used to store new representation")
  val minimalCoverage = parser.option[Double](List("minimalCoverage"), "double", "minimal coverage for a definition to be considered as large-coverage (percentage)")
  val newFacts = parser.option[String](List("newKB"), "filename", "a knowledge base to be mapped to the latent representation")
  val latentOutput = parser.option[String](List("latentOutput"), "filename", "file to save a transformed [newKB] representation")
  val numLayers = parser.option[Int](List("numLayers"), "n", "number of layers to create")
  val aggregatorFunctions = parser.option[String](List("aggregates"), "comma-separated list", "a list of aggregator functions to use for the numerical attributes [mean/min/max] ")
  val edgeCombination = parser.option[String](List("vertexCombination"), "[avg|min|max]", "how to combine values of vertex similarities in hyperedge?")
  val preserveOrder = parser.flag[Boolean](List("preserveVertexOrder"), "if set, preserves vertex order when clustering hyperedges")


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

    val agregates = aggregatorFunctions.value.getOrElse("mean").split(",").toList.foldLeft(List[AbstractAggregator]())( (acc, ag) => {
      ag match {
        case "mean" => acc :+ new AvgAggregator
        case "min" => acc :+ new MinAggregator
        case "max" => acc :+ new MaxAggregator
        case "sum" => acc :+ new SumAggregator
      }
    })

    val parameterSets = weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(":").toList.map( par => par.split(",").toList.map( _.toDouble ))
    val penalties = tradeOffFactor.value.getOrElse("0.1").contains(",") match {
      case true => tradeOffFactor.value.get.split(",").toList.map( _.toDouble)
      case false => List.fill(numLayers.value.getOrElse(1))(tradeOffFactor.value.get.toDouble)
    }

    require(penalties.length == numLayers.value.getOrElse(1), s"number of penalties ($penalties) does not match number of layers ${numLayers.value.getOrElse(1)}")

    var currentDeclarationsFile: String = declarationFile.value.get
    var currentHeaderFile: String = head.value.get
    var currentKnowledgeBasesFile: Seq[String] = dbs.value.get.split(",")

    var currentTestFactsFile = newFacts.value.get

    // build layer per layer
    (1 to numLayers.value.getOrElse(1)).foreach( nl => {
      val currentPredicateDeclarations = new PredicateDeclarations(currentDeclarationsFile)
      val currentKnowledgeBase = new KnowledgeBase(currentKnowledgeBasesFile, Helper.readFile(currentHeaderFile).mkString("\n"), currentPredicateDeclarations)
      val currentFolder = nl == 1 match {
        case false => s"${rootFolder.value.getOrElse("./layer")}$nl"
        case true => s"${rootFolder.value.getOrElse("./layer")}"
      }

      val currentFileName = s"${outputName.value.getOrElse("layer")}$nl"

      if (!new File(currentFolder).exists()) {
        new File(currentFolder).mkdir()
      }

      // clustering algorithm
      val clustering = algorithm.value.getOrElse("Spectral") match {
        case "Spectral" =>
          new Spectral(currentFolder)

        case "Hierarchical" =>
          new Hierarchical(linkage.value.getOrElse("average"), currentFolder)
      }

      //cluster validation
      val clusterValidationMethod = clusteringValidation.value.getOrElse("intraClusterSimilarity") match {
        case "silhouette" => new SilhouetteScore(currentFolder)
        case "intraClusterSimilarity" => new AverageIntraClusterSimilarity()
      }

      // cluster selection method
      val clusterSelector = selectionMethod.value.getOrElse("saturation") match {
        case "predefined" => new PredefinedNumber(clusterPerDomain.head)
        case "model" => new ModelBasedSelection(clusterValidationMethod)
        case "saturation" => new IncreaseSaturationCut(clusterValidationMethod, penalties(nl-1))
        case _ =>  new IncreaseSaturationCut(clusterValidationMethod, penalties(nl-1))
      }

      // measure for clustering overlap
      val clusterOverlap = new OverlapWithARI(currentFolder)

      try {

        val firstLayer = selectionMethod.value.getOrElse("saturation") match {
          case "saturation" => new AdaptiveSelectionLayer(currentFolder,
                                              currentFileName,
                                              currentKnowledgeBase,
                                              domainsToCluster,
                                              depth.value.getOrElse(0),
                                              bagComparison,
                                              bagCombinationMethod,
                                              agregates,
                                              preserveOrder.value.getOrElse(false),
                                              edgeCombination.value.getOrElse("avg"),
                                              clustering,
                                              clusterSelector,
                                              clusterOverlap,
                                              overlapThreshold.value.getOrElse(0.3),
                                              maxNumberOfClusters.value.getOrElse(10),
                                              parameterSets,
                                              clusterEdges.value.getOrElse(false),
                                              featureFormat.value.getOrElse(false)
          )
          case "model" => new MaxLayer(currentFolder,
                                      currentFileName,
                                      currentKnowledgeBase,
                                      domainsToCluster,
                                      depth.value.getOrElse(0),
                                      bagComparison,
                                      bagCombinationMethod,
                                      agregates,
                                      preserveOrder.value.getOrElse(false),
                                      edgeCombination.value.getOrElse("avg"),
                                      clustering,
                                      clusterValidationMethod,
                                      clusterOverlap,
                                      overlapThreshold.value.getOrElse(0.3),
                                      maxNumberOfClusters.value.getOrElse(10),
                                      parameterSets,
                                      clusterEdges.value.getOrElse(false),
                                      featureFormat.value.getOrElse(false))
        }

        val layerBuilder = new LayerBuilder(firstLayer, currentFolder, linkage.value.getOrElse("average"))
        val (latentHeader, latentDeclarations, latentKB) = layerBuilder.writeNewRepresentation(currentFileName)

        if (extractDefinitions.value.getOrElse(false)) {
          layerBuilder.writeDefinitions(definitionSupport.value.getOrElse(0.9), definitionDeviance.value.getOrElse(0.2))
        }

        if (newFacts.value.getOrElse("Nil") != "Nil") {
          layerBuilder.mapAndWriteFacts(currentTestFactsFile, currentHeaderFile, currentPredicateDeclarations, latentOutput.value.getOrElse(s"test-layer$nl.db"))
        }

        currentKnowledgeBasesFile = Seq(latentKB)
        currentDeclarationsFile = latentDeclarations
        currentHeaderFile = latentHeader
        currentTestFactsFile = s"$currentFolder/${latentOutput.value.getOrElse(s"test-layer$nl.db")}"
      }
      catch {
        case e: Exception => println(s"ERROR:layer$nl :: ${e.getMessage}\n \t ${e.getStackTrace}")
          val writeError = new PrintWriter(s"$currentFolder/error.log")
          e.printStackTrace(writeError)
          writeError.close()
      }
    })

    /*try {

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

      val layerBuilder = new LayerBuilder(firstLayer, rootFolder.value.getOrElse("./tmp"), linkage.value.getOrElse("average"))
      val (latentHeader, latentDeclarations, latentKB) = layerBuilder.writeNewRepresentation(outputName.value.getOrElse("newLayer") + "1")

      if (newFacts.value.getOrElse("Nil") != "Nil") {
        layerBuilder.mapAndWriteFacts(newFacts.value.get, head.value.get, predicateDeclarations, latentOutput.value.getOrElse(s"latent_representation.db"))
      }
    }
    catch {
      case e: Exception => println(s"ERROR: ${e.getMessage}\n \t ${e.getStackTrace}")
        val writeError = new PrintWriter(s"${rootFolder.value.getOrElse("./tmp")}/error.log")
        e.printStackTrace(writeError)
        writeError.close()
    }*/
  }
}
