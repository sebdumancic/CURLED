package deepSRL

import java.io.{BufferedWriter, FileWriter, File}

import deepSRL.basics.KnowledgeBase
import deepSRL.clustering.RelationalClusteringInitialization
import deepSRL.formats.Format
import deepSRL.utils.Helper

import org.clapper.argot.ArgotParser

/**
  * Created by seb on 17.12.15.
  */
object EnhancedDataset {

  //parser specifications
  import org.clapper.argot.ArgotConverters._

  val parser = new ArgotParser("EnhanceDataset.jar", preUsage = Some("Version 1.0"))
  val dbs = parser.multiOption[String](List("db"), "knowledgeBase", "database(s) with data to cluster")
  val head = parser.option[String](List("domain"), "domain definition", "header for the knowledge base(s); specification of logical predicates")
  val depth = parser.option[Int](List("depth"), "n", "depth of the neighbourhood graph")
  val rootFolder = parser.option[String](List("root"), "filePath", "folder to place files in")
  val k = parser.option[String](List("k"), "comma-separated ints", "number of clusters to create for each layer")
  val labels = parser.multiOption[String](List("labels"), "filePath", "file with labels [format: Class()]")
  val labelsHeader = parser.option[String](List("labelsDomain"), "domain definition of label predicates", "header for the knowledge base(s); specification of logical predicates")
  val scaleFactors = parser.option[String](List("scale"), "Array[Double]", "comma-separated list of scaling factors (4 numbers that sum to 1)")
  val overlapType = parser.option[String](List("overlap"), "[union|min|max|histogram]", "normalization of overlapping measure")
  val format = parser.option[String](List("format"), "MLN|tilde", "format for the target database")
  val numLayers = parser.option[Int](List("numLayers"), "n", "number of layers to construct")


  def prepareDataset(knowledgeBase: KnowledgeBase, k: Int, layerDepth: Int, folderRoot: String, layer: Int, format: String = "MLN", overlap: String = "histogram") = {
    val queries = collection.mutable.Set[String]()

    val clusterOutput = new File(s"$folderRoot/cluster")
    if (!clusterOutput.exists()) { clusterOutput.mkdir() }

    val cluster = new RelationalClusteringInitialization(knowledgeBase, layerDepth, normalize = true, folderRoot + "/cluster", k, folderRoot + "/clustering.log", overlapMeasure = overlap)

    val newHeaderFile = new FileWriter(folderRoot + s"/new_header_layer$layer.def")
    val newKBFile = new FileWriter(folderRoot + s"/new_kb_layer$layer.kb")

    //all single domains + all connections in knowledge base (not all possible)
    knowledgeBase.getAllDomains.keys.foreach( queries += _ )
    knowledgeBase.getPredicateNames.map( knowledgeBase.getPredicate ).filter( _.arity > 1).foreach( predicate => {
      predicate.getDomains.combinations(2).foreach( domComb => queries += domComb.mkString(","))
    })

    try {
      queries.foreach(query => {
        val domDesc = query.replace(",", "")
        try {
          cluster.getAllClusters(query.split(",").toList, subSample = false).zipWithIndex.foreach(clusterContent => {
            newHeaderFile.write(format match {
              case "tilde" => Format.tildeFormatFact(s"Cluster_" + clusterContent._2 + s"_$domDesc", query) + "\n"
              case "MLN" => Format.MLNFormatDefinition(s"Cluster_" + clusterContent._2 + s"_$domDesc", query) + "\n"
            })

            newKBFile.write(clusterContent._1.map(x => format match {
              case "tilde" => Format.tildeFormatFact(s"Cluster_" + clusterContent._2 + s"_$domDesc", x)
              case "MLN" => Format.MLNFormatFact(s"Cluster_" + clusterContent._2 + s"_$domDesc", x)
            }).mkString("\n") + "\n")

          })
        }
        catch {
          case e: Exception => println(s"Couldn't cluster query $query"); println(e.getMessage + "\n" + e.getLocalizedMessage + "\n" + e.getCause)
        }
      })
    }
    catch {
      case e: Exception => println("Something wrong happened: " + e.getCause)
    }
    finally {
      newHeaderFile.close()
      newKBFile.close()
    }

    new KnowledgeBase(Seq(s"$folderRoot/new_kb_layer$layer.kb"), Helper.readFile(s"$folderRoot/new_header_layer$layer.def").mkString("\n"))
  }

  def prepareTildeSettings(filename: String, kbase: KnowledgeBase, testKB: KnowledgeBase, heuristic: String = "gain", minCases: Int = 10) = {
    val writer = new BufferedWriter(new FileWriter(filename))

    try {
      writer.write("load(key).\n")
      writer.write("talking(0).\n")
      writer.write("output_options([c45,prolog]).\n")
      writer.write("pruning(none).\n")
      writer.write(s"heuristic($heuristic).\n")
      writer.write("bias(warmode).\n")
      writer.write("tilde_mode(classify).\n")
      writer.write("classes([" + testKB.getPredicateNames.map(_.toLowerCase).mkString(",") + "]).\n")
      writer.write(s"minimal_cases($minCases).\n")
      writer.write("\n")

      //target predicate
      writer.write("predict(target(+" + testKB.getPredicateNames.map(testKB.getPredicate).head.getDomains.mkString(",+") + ",-class)).\n")
      writer.write("\n")

      //types
      writer.write("typed_language(yes).\n\n")
      kbase.getPredicateNames.map(kbase.getPredicate).foreach(predicate => {
        writer.write("warmode(" + Format.tildeFormatDefinition(predicate.getName, predicate.getDomains.mkString(",")) + ").\n")
        writer.write("type(" + Format.tildeFormatDefinition(predicate.getName, predicate.getDomains.mkString(",")) + ").\n")
      })
    }
    catch {
      case e: Exception => println(s"couldn't prepare settings for $filename"); println(e.getCause + "\n" + e.getMessage + "\n" + e.getStackTrace)
    }
    finally {
      writer.close()
    }
  }

  def formatKnowledgeBase(knowledgeBase: KnowledgeBase, filename: String, format: String = "tilde") = {
    knowledgeBase.printToFile(filename, format)
  }

  def buildLayers(knowledgeBase: KnowledgeBase, testKB: KnowledgeBase, numLayers: Int, k: Seq[Int], layerDepth: Int, overlap: String = "histogram", root: String = ".") = {
    var currentKB = knowledgeBase
    prepareTildeSettings(root + s"/tilde_settings_original.s", currentKB, testKB)
    formatKnowledgeBase(currentKB, root + s"/tilde_original.kb", "tilde")

    (0 until numLayers).foreach( layer => {
      currentKB = prepareDataset(currentKB, k(layer), layerDepth, root, layer, overlap = overlap)
      prepareTildeSettings(root + s"/tilde_settings_layer$layer.s", currentKB, testKB)
      formatKnowledgeBase(currentKB, root + s"/tilde_layer$layer.kb", "tilde")
    })
  }

  def main(args: Array[String]) {
    parser.parse(args)
    require(dbs.value.nonEmpty, "databases not provided")
    require(labels.value.nonEmpty, "test databases not provided")
    require(head.hasValue, "header not provided")
    require(labelsHeader.hasValue, "test predicate definitions not provided")

    val kBase = new KnowledgeBase(dbs.value, Helper.readFile(head.value.get).mkString("\n"))
    val testKB = new KnowledgeBase(labels.value, Helper.readFile(labelsHeader.value.get).mkString("\n"))

    buildLayers(kBase, testKB, numLayers.value.getOrElse(0), k.value.getOrElse("k").split(",").map( _.toInt), depth.value.getOrElse(0),
                overlapType.value.getOrElse("histogram"), rootFolder.value.getOrElse("."))


  }



}
