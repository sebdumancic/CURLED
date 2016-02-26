package representationLearning

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import learners.ilp.{TildeInduce, TildeNFold}
import org.clapper.argot.ArgotParser
import relationalClustering.bagComparison.bagCombination.{IntersectionCombination, UnionCombination}
import relationalClustering.bagComparison.{ChiSquaredDistance, MaximumSimilarity, MinimumSimilarity, Unionsimilarity}
import relationalClustering.clustering.evaluation.{AverageIntraClusterSimilarity, SilhouetteScore}
import relationalClustering.clustering.{Hierarchical, Spectral}
import relationalClustering.representation.KnowledgeBase
import relationalClustering.similarity.{SimilarityNTv2, SimilarityNeighbourhoodTrees}
import relationalClustering.utils.{Helper, PredicateDeclarations, Settings}
import representationLearning.clusterComparison.OverlapWithARI
import representationLearning.clusterSelection.{IncreaseSaturationCut, ModelBasedSelection, PredefinedNumber}

/**
  * CLI implementing the functionality of learning new representation with
  * Created by seb on 09.02.16.
  */
object LearnNewRepresentation {

  //parser specification
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser("LearnNewRepresentation.jar", preUsage = Some("Version 1.1"))
  val dbs = parser.multiOption[String](List("db"), "knowledgeBase", "database(s) with data to cluster")
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
  val selectionMethod = parser.option[String](List("selection"), "predefined|model|saturation", "how to select the number of clusters per domain")
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


  /** Checks whether a hyperEdge between specified domains exists in a knowledge base
    *
    * @param doms list of domains in a hyperEdge
    * @return [[Boolean]]
    * */
  def existsConnection(doms: List[String], knowledgeBase: KnowledgeBase) = {
    knowledgeBase.getPredicateNames.map(knowledgeBase.getPredicate).filter( _.getRole == Settings.ROLE_HYPEREDGE).foldLeft(false)( (acc, pred) => {
      acc || pred.getDomains.sorted.combinations(doms.length).map(_.toList).contains(doms)
    })
  }

  def printParameters() = {
    println("CLUSTERING WITH THE FOLLOWING PARAMETERS")
    println(s"---- depth: ${depth.value.getOrElse(0)}")
    println(s"---- parameter set: ${weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2")}")
    println(s"---- clustering algorithm: ${algorithm.value.getOrElse("Spectral")}")
    println(s"---- similarity measure: ${similarity.value.getOrElse("RCNT")}")
    println(s"---- bag similarity measure: ${bag.value.getOrElse("chiSquared")}")
    println(s"---- bag combination method: ${bagCombination.value.getOrElse("intersection")}")
    println(s"---- linkage (for Hierarchical clustering): ${linkage.value.getOrElse("average")}")
    println(s"---- using local directory: ${useLocalRepository.value.getOrElse(false)}")
    println(s"---- maximal number of clusters: ${maxNumberOfClusters.value.getOrElse(10)}")
    println(s"---- saving everything with name: ${outputName.value.getOrElse("newLayer")}")
    println()
    println(s"---- query: ${query.value.get}")
    println(s"---- clustering selection method: ${selectionMethod.value.getOrElse("predefined")}")
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
          println(s"---- tilde minimal number of cases: ${tildeMinCases.value.getOrElse(2)}")
        case "TildeNFold" =>
          println(s"---- tilde heuristics: ${tildeHeuristic.value.getOrElse("gain")}")
          println(s"---- tilde minimal number of cases: ${tildeMinCases.value.getOrElse(2)}")
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
    val KnowledgeBase = new KnowledgeBase(dbs.value, Helper.readFile(head.value.get).mkString("\n"), predicateDeclarations)

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
      case "union" => new Unionsimilarity()
    }

    val bagCombinationMethod = bagCombination.value.getOrElse("intersection") match {
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


    val kbWriter = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.db"))
    val headerWriter = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.def"))
    val declarationsWriter = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.decl"))

    val parameterSets = weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(":").toList.map( par => par.split(",").toList.map( _.toDouble ))

    try {
      val onset = maxNumberOfClusters.value.getOrElse(10)

      val clusterOverlapMeasure = new OverlapWithARI(rootFolder.value.getOrElse("./tmp"))

        //CREATING NEW REPRESENTATION, PER EACH DOMAIN
        domainsToCluster.zip(clusterPerDomain).foreach(domain => {
          println(s"Clustering domains ${domain._1}")

          val allCreatedClusters = collection.mutable.Set[Set[List[String]]]()

          parameterSets.zipWithIndex.foreach(params => {
            println(s"---- Clustering with the following parameters ${params._1}")

            val similarityMeasure = similarity.value.getOrElse("RCNT") match {
              case "RCNT" =>
                new SimilarityNeighbourhoodTrees(KnowledgeBase,
                  depth.value.getOrElse(0),
                  params._1,
                  bagComparison,
                  bagCombinationMethod,
                  useLocalRepository.value.getOrElse(false))
              case "RCNTv2" =>
                new SimilarityNTv2(KnowledgeBase,
                  depth.value.getOrElse(0),
                  params._1,
                  bagComparison,
                  bagCombinationMethod,
                  useLocalRepository.value.getOrElse(false))
            }

            // build all clusterings
            var createdClusters = List[Set[List[String]]]()
            val filename = similarityMeasure.getObjectSimilaritySave(List(domain._1), rootFolder.value.getOrElse("./tmp"))

            (2 until maxNumberOfClusters.value.getOrElse(10)).foreach(numCl => {
              createdClusters = createdClusters :+ clustering.clusterFromFile(filename._1, math.min(numCl, filename._2.length - 1))
            })


            // cluster selection method
            val clusterSelector = selectionMethod.value.getOrElse("predefined") match {
              case "predefined" => new PredefinedNumber(domain._2)
              case "model" => new ModelBasedSelection(filename._1, filename._2.map(_._1), clusterValidationMethod)
              case "saturation" => new IncreaseSaturationCut(filename._1, filename._2.map(_._1), clusterValidationMethod, tradeOffFactor.value.getOrElse(0.9))
            }


            // select best clustering, and write to file
            val selectedCluster = clusterSelector.selectFromClusters(createdClusters)

            allCreatedClusters.nonEmpty match {
              case true =>
                val maxOverlap = allCreatedClusters.map(cl => clusterOverlapMeasure.compare(cl, selectedCluster)).max
                if (maxOverlap < overlapThreshold.value.getOrElse(0.3)) {
                  allCreatedClusters += selectedCluster
                  println(s"---- ---- ---- Cluster accepted ($maxOverlap)")
                }
                else {
                  println(s"---- ---- ---- Cluster rejected because $maxOverlap: $params, $domain")
                }
              case false =>
                allCreatedClusters += selectedCluster
            }

            //allCreatedClusters += selectedCluster
            /*selectedCluster.zipWithIndex.foreach(clust => {
              headerWriter.write(s"Cluster_${domain._1}${clust._2 + (params._2 * onset)}(${domain._1})\n")
              declarationsWriter.write(s"Cluster_${domain._1}${clust._2 + (params._2 * onset)}(name)\n")
              kbWriter.write(clust._1.map(elem => s"Cluster_${domain._1}${clust._2 + (params._2 * onset)}($elem)").mkString("\n") + "\n")
            })*/

            // clear the cache for the next domain
            similarityMeasure.clearCache()

            // additional newline for easier reading
            /*headerWriter.write(s"\n")
            declarationsWriter.write(s"\n")
            kbWriter.write("\n")

            headerWriter.flush()
            declarationsWriter.flush()
            kbWriter.flush()*/
          })

          allCreatedClusters.zipWithIndex.foreach(clustering => {
            clustering._1.zipWithIndex.foreach(clust => {
              headerWriter.write(s"Cluster_${domain._1}${clust._2 + (clustering._2 * onset)}(${domain._1})\n")
              declarationsWriter.write(s"Cluster_${domain._1}${clust._2 + (clustering._2 * onset)}(name)\n")
              kbWriter.write(clust._1.map(elem => s"Cluster_${domain._1}${clust._2 + (clustering._2 * onset)}($elem)").mkString("\n") + "\n")
            })

            headerWriter.write(s"\n")
            declarationsWriter.write(s"\n")
            kbWriter.write("\n")
          })

          headerWriter.flush()
          declarationsWriter.flush()
          kbWriter.flush()

          allCreatedClusters.clear()
        })


        // CLUSTER LINKS BETWEEN THESE DOMAINS
        if (clusterEdges.value.getOrElse(false)) {

          (domainsToCluster ++ domainsToCluster).sorted.combinations(2).filter(com => existsConnection(com, KnowledgeBase)).foreach(comb => {
            println(s"Clustering hyperedge $comb")

            parameterSets.zipWithIndex.foreach(params => {
              println(s"---- Clustering with the following parameters ${params._1}")

              val similarityMeasure = similarity.value.getOrElse("RCNT") match {
                case "RCNT" =>
                  new SimilarityNeighbourhoodTrees(KnowledgeBase,
                    depth.value.getOrElse(0),
                    params._1,
                    bagComparison,
                    bagCombinationMethod,
                    useLocalRepository.value.getOrElse(false))
                case "RCNTv2" =>
                  new SimilarityNTv2(KnowledgeBase,
                    depth.value.getOrElse(0),
                    params._1,
                    bagComparison,
                    bagCombinationMethod,
                    useLocalRepository.value.getOrElse(false))
              }

              var createdClusters = List[Set[List[String]]]()
              val filename = similarityMeasure.getHyperEdgeSimilaritySave(comb, rootFolder.value.getOrElse("./tmp"))

              (2 until maxNumberOfClusters.value.getOrElse(10)).foreach(numCl => {
                createdClusters = createdClusters :+ clustering.clusterFromFile(filename._1, math.min(numCl, filename._2.length - 1))
              })

              // cluster selection method
              val clusterSelector = selectionMethod.value.getOrElse("predefined") match {
                case "predefined" => new PredefinedNumber(k.value.getOrElse(2))
                case "model" => new ModelBasedSelection(filename._1, filename._2.map(_.mkString(":")), clusterValidationMethod)
                case "saturation" => new IncreaseSaturationCut(filename._1, filename._2.map(_.mkString(":")), clusterValidationMethod, tradeOffFactor.value.getOrElse(0.9))
              }

              val selectedCluster = clusterSelector.selectFromClusters(createdClusters)
              selectedCluster.zipWithIndex.foreach(clust => {
                headerWriter.write(s"Cluster_${comb.mkString("_")}${clust._2 + (params._2 * onset)}(${comb.mkString(",")})\n")
                declarationsWriter.write(s"Cluster_${comb.mkString("_")}${clust._2 + (params._2 * onset)}(${comb.map(x => "name").mkString(",")})\n")
                kbWriter.write(clust._1.map(elem => s"Cluster_${comb.mkString("_")}${clust._2 + (params._2 * onset)}(${elem.replace(":", ",")})").mkString("\n") + "\n")
              })

              // clear the cache for the next domain
              similarityMeasure.clearCache()

              // additional newline for easier reading
              headerWriter.write(s"\n")
              declarationsWriter.write(s"\n")
              kbWriter.write("\n")

              headerWriter.flush()
              declarationsWriter.flush()
              kbWriter.flush()
            })
          })
        }



      //extract definitions of discovered predicates
      if (extractDefinitions.value.getOrElse(false)) {
        val latentPredicateDeclarations = new PredicateDeclarations(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.decl")
        val latentKB = new KnowledgeBase(Seq(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.db"),
                                         Helper.readFile(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.def").mkString("\n"),
                                         latentPredicateDeclarations)

        println("\n\n\n FOUND PREDICATES")
        latentKB.getPredicateNames.map(latentKB.getPredicate).foreach( pred => {

          val learner = defLearner.value.getOrElse("TildeInduce") match {
            case "TildeInduce" => new TildeInduce(rootFolder.value.getOrElse("./tmp"), KnowledgeBase, latentKB, pred.getName, pred.getRole == Settings.ROLE_HYPEREDGE,
                                                  sys.env.getOrElse("ACE_ILP_ROOT", "/home/seba/Software/ACE-ilProlog-1.2.20/linux"), tildeHeuristic.value.getOrElse("gain"),
                                                  tildeMinCases.value.getOrElse(2))
            case "TildeNFold" => new TildeNFold(rootFolder.value.getOrElse("./tmp"), KnowledgeBase, latentKB, pred.getName, pred.getRole == Settings.ROLE_HYPEREDGE,
                                                sys.env.getOrElse("ACE_ILP_ROOT", "/home/seba/Software/ACE-ilProlog-1.2.20/linux"), nFold.value.getOrElse(10),
                                                tildeHeuristic.value.getOrElse("gain"), tildeMinCases.value.getOrElse(2))
          }

          learner.fitModel()

          println("*"*10)
          learner.getDefinitions.foreach(pd => println(s"  ${pd.getAbsCoverage}(${pd.getRelCoverage}) ::  $pd"))
          println("*"*10)

        })
      }
    }
    catch {
      case e: Exception => println(s"ERROR: ${e.getMessage}\n \t ${e.getStackTrace}")
        val writeError = new PrintWriter(s"${rootFolder.value.getOrElse("./tmp")}/error.log")
        e.printStackTrace(writeError)
        writeError.close()
    }
    finally {
      kbWriter.close()
      headerWriter.close()
      declarationsWriter.close()
    }
  }
}
