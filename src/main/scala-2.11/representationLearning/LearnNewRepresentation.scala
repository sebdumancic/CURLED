package representationLearning

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import org.clapper.argot.ArgotParser
import relationalClustering.bagComparison.bagCombination.{IntersectionCombination, UnionCombination}
import relationalClustering.bagComparison.{ChiSquaredDistance, MaximumSimilarity, MinimumSimilarity, Unionsimilarity}
import relationalClustering.clustering.evaluation.SilhouetteScore
import relationalClustering.clustering.{Hierarchical, Spectral}
import relationalClustering.representation.KnowledgeBase
import relationalClustering.similarity.{SimilarityNTv2, SimilarityNeighbourhoodTrees}
import relationalClustering.utils.{Helper, PredicateDeclarations}
import representationLearning.clusterSelection.{ModelBasedSelection, PredefinedNumber}

/**
  * CLI implementing the functionality of learning new representation with
  * Created by seb on 09.02.16.
  */
object LearnNewRepresentation {

  //parser specification
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser("LearnNewRepresentation.jar", preUsage = Some("Version 1.0"))
  val dbs = parser.multiOption[String](List("db"), "knowledgeBase", "database(s) with data to cluster")
  val head = parser.option[String](List("domain"), "domain definition", "header for the knowledge base(s); specification of logical predicates")
  val declarationFile = parser.option[String](List("declarations"), "file path", "file containing declarations of predicates")
  val depth = parser.option[Int](List("depth"), "n", "depth of the neighbourhood graph")
  val rootFolder = parser.option[String](List("root"), "filePath", "folder to place files in")
  val weights = parser.option[String](List("weights"), "Array[Double]", "comma-separated list of weights [attributes,attribute distribution,connections,vertex neighbourhood, edge distribution]")
  val algorithm = parser.option[String](List("algorithm"), "[Spectral|Hierarchical]", "algorithm to perform clustering")
  val similarity = parser.option[String](List("similarity"), "[RCNT|RCNTv2|HS|RIBL|HSAG]", "similarity measure")
  val bag = parser.option[String](List("bagSimilarity"), "[chiSquared|maximum|minimum|union]", "bag similarity measure")
  val bagCombination = parser.option[String](List("bagCombination"), "[union|intersection]", "bag combination method")
  val linkage = parser.option[String](List("linkage"), "[average|complete|ward]", "linkage for hierarchical clustering")
  val useLocalRepository = parser.flag[Boolean](List("localRepo"), "should NodeRepository be constructed locally for each NeighbourhoodGraph, or one globally shared")
  val query = parser.option[String](List("query"), "comma-separated list", "list of domains to cluster; if not set, all domains are clustered")
  val maxNumberOfClusters = parser.option[Int](List("maxClusters"), "n", "maximal number of clusters to create, per domain")
  val selectionMethod = parser.option[String](List("selection"), "predefined|silhouette", "how to select the number of clusters per domain")
  val outputName = parser.option[String](List("output"), "string", "name of the file to save new layer [default:newLayer.*]")
  val k = parser.option[Int](List("k"), "n", "desired number of clusters in 'predefined' selection method is used")
  val kPerDomain = parser.option[String](List("kDomain"), "comma-separated list of domain:numClusters", "number of clusters per domain")
  val clusterEdges = parser.flag[Boolean](List("clusterHyperedges"), "should hyperedges be clusters as well (between the specified domains)")

  def main(args: Array[String]) {
    parser.parse(args)
    require(head.hasValue, "no header specified")
    require(dbs.hasValue, "no databases specified")
    require(query.hasValue, "query not specified")
    require(declarationFile.hasValue, "declarations of the predicates not provided")

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

    val similarityMeasure = similarity.value.getOrElse("RCNT") match {
      case "RCNT" =>
        new SimilarityNeighbourhoodTrees(KnowledgeBase,
                                         depth.value.getOrElse(0),
                                         weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(",").toList.map(_.toDouble),
                                         bagComparison,
                                         bagCombinationMethod,
                                         useLocalRepository.value.getOrElse(false))
      case "RCNTv2" =>
        new SimilarityNTv2(KnowledgeBase,
                           depth.value.getOrElse(0),
                           weights.value.getOrElse("0.2,0.2,0.2,0.2,0.2").split(",").toList.map(_.toDouble),
                           bagComparison,
                           bagCombinationMethod,
                           useLocalRepository.value.getOrElse(false))
    }

    // clustering algorithm
    val clustering = algorithm.value.getOrElse("Spectral") match {
      case "Spectral" =>
        new Spectral(rootFolder.value.getOrElse("./tmp"))

      case "Hierarchical" =>
        new Hierarchical(linkage.value.getOrElse("average"), rootFolder.value.getOrElse("./tmp"))
    }

    val kbWriter = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.kb"))
    val headerWriter = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.def"))
    val declarationsWriter = new BufferedWriter(new FileWriter(s"${rootFolder.value.getOrElse("./tmp")}/${outputName.value.getOrElse("newLayer")}.decl"))


    try {
      //CREATING NEW REPRESENTATION, PER EACH DOMAIN
      domainsToCluster.zip(clusterPerDomain).foreach(domain => {

        // build all clusterings
        var createdClusters = List[Set[List[String]]]()
        val filename = similarityMeasure.getObjectSimilaritySave(List(domain._1), rootFolder.value.getOrElse("./tmp"))

        (2 until maxNumberOfClusters.value.getOrElse(10)).foreach(numCl => {
          createdClusters = createdClusters :+ clustering.clusterFromFile(filename._1, numCl)
        })


        // cluster selection method
        val clusterSelector = selectionMethod.value.getOrElse("predefined") match {
          case "predefined" => new PredefinedNumber(domain._2)
          case "silhouette" =>
            val clusterEvaluation = new SilhouetteScore(rootFolder.value.getOrElse("./tmp"))
            new ModelBasedSelection(filename._1, filename._2.map(_._1), clusterEvaluation)
        }


        // select best clustering, and write to file
        val selectedCluster = clusterSelector.selectFromClusters(createdClusters)
        selectedCluster.zipWithIndex.foreach(clust => {
          headerWriter.write(s"Cluster_${domain._1}${clust._2}(${domain._1})\n")
          declarationsWriter.write(s"Cluster_${domain._1}${clust._2}(name)\n")
          kbWriter.write(clust._1.map(elem => s"Cluster_${domain._1}${clust._2}($elem)").mkString("\n") + "\n")
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


      // CLUSTER LINKS BETWEEN THESE DOMAINS
      if (clusterEdges.value.getOrElse(false) && domainsToCluster.length > 1) {

        domainsToCluster.sorted.combinations(2).foreach(comb => {
          var createdClusters = List[Set[List[String]]]()
          val filename = similarityMeasure.getHyperEdgeSimilaritySave(comb, rootFolder.value.getOrElse("./tmp"))

          (2 until maxNumberOfClusters.value.getOrElse(10)).foreach(numCl => {
            createdClusters = createdClusters :+ clustering.clusterFromFile(filename._1, numCl)
          })

          // cluster selection method
          val clusterSelector = selectionMethod.value.getOrElse("predefined") match {
            case "predefined" => new PredefinedNumber(k.value.getOrElse(2))
            case "silhouette" =>
              val clusterEvaluation = new SilhouetteScore(rootFolder.value.getOrElse("./tmp"))
              new ModelBasedSelection(filename._1, filename._2.map(_.mkString(",")), clusterEvaluation)
          }

          val selectedCluster = clusterSelector.selectFromClusters(createdClusters)
          selectedCluster.zipWithIndex.foreach(clust => {
            headerWriter.write(s"Cluster_${comb.mkString("_")}${clust._2}(${comb.mkString(",")})\n")
            declarationsWriter.write(s"Cluster_${comb.mkString("_")}${clust._2}(${comb.map(x => "name")})\n")
            kbWriter.write(clust._1.map(elem => s"Cluster_${comb.mkString("_")}${clust._2}($elem)").mkString("\n") + "\n")
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
