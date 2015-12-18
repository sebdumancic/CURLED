package deepSRL.clustering

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Calendar

import deepSRL.basics.{Predicate, KnowledgeBase}
import deepSRL.utils.{Helper, HistogramDistance, Histogram}

import scala.sys.process._
import scala.util.Random

import breeze.linalg._


/**
 * Implements distance measure function for clustering initialization
 *
 * Created by seb on 13.10.15.
 */
class RelationalClusteringInitialization( val knowledgeBase: KnowledgeBase,
                                          val jumpStep: Int,
                                          val normalize: Boolean,
                                          val rootFolder: String,
                                          val k: Int,
                                          val logFile: String,
                                          val scaleFactors: Array[Double] = Array[Double](0.2,0.2,0.2,0.2,0.2),
                                          val overlapMeasure: String = "union",
                                          val algorithm: String = "Spectral") {

  private val clusteringFile = "relclustering.py"
  createPythonScript(rootFolder + "/" + clusteringFile)
  require(scaleFactors.sum == 1.0, s"scale factors don't sum to 1 $scaleFactors")
  require(scaleFactors.length == 5, s"number of scale factors does not match number of matrices $scaleFactors")
  println(s"CLUSTERING normalize: $getNormalization\nCLUSTERING k: $getK\nCLUSTERING depth: $getJumpStep\nCLUSTERING algorithm: $getAlgorithm\nCLUSTERING overlap: $getOverlapMeasure\n")

  private val createdClusters = collection.mutable.Map[String, List[List[String]]]()
  private val lastIndexedCluster = collection.mutable.Map[String, Int]()
  private val accessingClustersCounter = collection.mutable.Map[String, Int]()
  private val neighbourhoodGraphCache = collection.mutable.Map[(String,String,Int), NeighbourhoodGraph]() // [element, Domain, Depth]
  private val attributeNodesCache = collection.mutable.Map[String, List[String]]() // [ObjectName(Node) -> List[Attributes] ]
  createAttributeNodes()

  private def getKnowledgeBase = { knowledgeBase }
  private def getJumpStep = { jumpStep }
  private def getNormalization = { normalize }
  private def getRootFolder = { rootFolder }
  def getClusteringFile = { getRootFolder + "/" + clusteringFile }
  private def getLogFile = { logFile }
  private def getK = { k }
  private def getNeighbourhoodGraph(element: String, domain: String, depth: Int) = {
    val accessIndex = new Tuple3(element, domain, depth)
    if (!neighbourhoodGraphCache.contains(accessIndex)) { neighbourhoodGraphCache(accessIndex) = new NeighbourhoodGraph(element, domain, depth, getKnowledgeBase) }
    neighbourhoodGraphCache(accessIndex)
  }
  private def getAnyNeighbourhoodGraph(element: String, domain: String) = {
    val index = neighbourhoodGraphCache.keys.toList.filter( x => x._2 == domain && x._1 == element).sortBy( _._3)
    if (index.isEmpty) { getNeighbourhoodGraph(element, domain, 0)}
    else { getNeighbourhoodGraph(index.head._1, index.head._2, index.head._3)}
  }

  def getOverlapMeasure = { overlapMeasure }
  def getAlgorithm = { algorithm }
  def addAttributeToNode(domain: String, element: String, attr: String) = {
    if (!attributeNodesCache.contains(element)) { attributeNodesCache(element) = List[String]()}
    attributeNodesCache(element) = attributeNodesCache(element) :+ attr
  }
  def getAttributes(domain: String, element: String) = {
    attributeNodesCache.getOrElse(element, List[String]())
  }

  private def increaseAccessCount(key: String) = { if (!accessingClustersCounter.contains(key)) { accessingClustersCounter(key) = 0}; accessingClustersCounter(key) += 1 }
  def roundMade(domains: List[String]) = { accessingClustersCounter(domains.mkString(",")) >= createdClusters(domains.mkString(",")).size}

  private def getInputFileName(domains: List[String]) = { getRootFolder + "/" + domains.mkString("_") + s"_depth$getJumpStep"  + s"_normalize$getNormalization" + "_scale" + scaleFactors.mkString(",") + s"$getOverlapMeasure" + ".txt"}
  private def getClustersFilename(domains: List[String]) = { getRootFolder + "/clusters" + domains.mkString("_") + s"_depth$getJumpStep" + s"_normalize$getNormalization" + s"_k$getK" + "_scale" + scaleFactors.mkString(",") + s"$getOverlapMeasure" + ".txt"}

  private def addCreatedCluster(key: String, cluster: List[String]) = {
    if (!getCreatedClusters.contains(key)) { createdClusters(key) = List[List[String]](); lastIndexedCluster(key) = 0}
    createdClusters(key) = createdClusters(key) :+ cluster
  }
  private def getNextClusterIndex(key: String) = { lastIndexedCluster(key) = (lastIndexedCluster(key) + 1) % createdClusters(key).size; math.max(0, lastIndexedCluster(key) - 1) }
  private def getNextCluster(key: String) = { increaseAccessCount(key); createdClusters(key)(getNextClusterIndex(key)) }
  def getCreatedClusters = { createdClusters.keySet }
  def nextClusterIndex(domains: List[String]) = { lastIndexedCluster(domains.mkString(",")) + 1}

  private def getScript = {
    """import numpy as np
      |from sklearn.cluster import DBSCAN, AffinityPropagation, SpectralClustering, AgglomerativeClustering
      |import argparse
      |
      |__author__ = 'seb'
      |
      |parser = argparse.ArgumentParser(description='Read arguments for clustering.')
      |parser.add_argument('--alg', help='Algorithm to cluster [Affinity|DBscan]', nargs=1, default=['Affinity'], choices=['Affinity', 'DBscan', 'Spectral', 'Agglomerative'])
      |parser.add_argument('--input', help='filename containing distance|similarity matrix', nargs=1, required=True)
      |parser.add_argument('--output', help='filename for the resulting clustering', nargs=1, required=True)
      |parser.add_argument('--eps', help='[DBscan] epsilon parameter', nargs=1, default=[0.3], type=float)
      |parser.add_argument('--damping', help='[Affinity] dumping factor', nargs=1, default=[0.65], type=float)
      |parser.add_argument('--pref', help='[Affinity] preference', nargs=1, default=[None], type=float)
      |parser.add_argument('--k', help='[Spectral] number of clusters to find', nargs=1, default=[3], type=int)
      |
      |args = parser.parse_args()
      |
      |algorithm = args.alg[0]
      |inputFile = args.input[0]
      |outputClusters = args.output[0]
      |
      |distanceMatrix = np.loadtxt(inputFile, delimiter=";", comments="#")
      |domainObjects = map(lambda x: x.strip(), open(inputFile).readline().replace("#", "").split(";"))
      |
      |if algorithm == "DBscan":
      |    clusters = DBSCAN(eps=args.eps[0], min_samples=max(int(len(domainObjects) * 0.1), 2), metric='precomputed', algorithm='auto').fit(distanceMatrix)
      |elif algorithm == "Affinity" and args.pref:
      |    clusters = AffinityPropagation(damping=args.damping[0], affinity='precomputed', preference=args.pref[0]).fit(distanceMatrix)
      |elif algorithm == "Affinity":
      |    clusters = AffinityPropagation(damping=args.damping[0], affinity='precomputed').fit(distanceMatrix)
      |elif algorithm == "Spectral":
      |    ktoUse = min([args.k[0], np.linalg.matrix_rank(distanceMatrix) - 1])
      |    print " using k={} instead of k={}".format(ktoUse, args.k[0])
      |    clusters = SpectralClustering(n_clusters=ktoUse, affinity='precomputed').fit(distanceMatrix)
      |elif algorithm == 'Agglomerative':
      |    distance = 1.0 - np.divide(distanceMatrix, distanceMatrix.max())
      |    clusters = AgglomerativeClustering(n_clusters=args.k[0], affinity='precomputed', linkage='average').fit(distance)
      |else:
      |    print "ERROR: no {} clustering procedure, performing DBSCAN".format(algorithm)
      |    clusters = DBSCAN(eps=0.2, min_samples=max(int(len(domainObjects) * 0.1), 2), metric='precomputed', algorithm='auto').fit(distanceMatrix)
      |
      |elementsInCluster = {}
      |
      |for (element, cluster) in zip(domainObjects, clusters.labels_):
      |    if cluster not in elementsInCluster:
      |        elementsInCluster[cluster] = []
      |    elementsInCluster[cluster].append(element)
      |
      |writer = open(outputClusters, 'w')
      |
      |oneBig = []
      |
      |for item in elementsInCluster:
      |    if len(elementsInCluster[item]) > 1:
      |        writer.write("{}=".format(item) + "{" + ";".join(elementsInCluster[item]) + "}\n")
      |    else:
      |        oneBig.append(elementsInCluster[item][0])
      |
      |if len(oneBig) > 0:
      |    writer.write("{}=".format(len(elementsInCluster)) + "{" + ";".join(oneBig) + "}\n")
      |writer.close()
      |
      |
    """.stripMargin
  }

  private def createPythonScript(name: String) = {
    val writer = new FileWriter(name)
    writer.write(getScript)
    writer.close()
  }

  private def createAttributeNodes() = {
    getKnowledgeBase.getPredicateNames.map( getKnowledgeBase.getPredicate ).filter( _.arity == 1).foreach( predicate => {
      val predDomain = predicate.getDomains.head
      predicate.getTrueGroundings.foreach( grounding => {
        addAttributeToNode(predDomain, grounding.head, predicate.getName)
      })
    })
  }

  def getSimilarity(domains: List[String], subSample: Boolean = true, proportion: Double = 0.5) = {
    val domainsToCombine = subSample match {
      case false => domains.map(getKnowledgeBase.getDomain(_).getElementsAsList)
      case true => println("CLUSTERING: subSampling domains..."); domains.map( x => subSampling(x, proportion))
    }

    val domainElements = domainsToCombine.reduceLeft((x, y) => {
      for {xs <- x; ys <- y} yield xs ::: ys }).filter( x => x.toSet.size == domains.length ).map( x => { if ( domains.toSet.size == domains.length) {x} else {x.sorted}}).toList

    calculateSimilarityMatrix(getKnowledgeBase, domainElements, domains)
  }

  def subSampling(domain: String, proportion: Double = 0.5) = {
    val domainElements = getKnowledgeBase.getDomain(domain).getElementsAsList.toList

    if (domainElements.size >= getK * 2) {

      val kToUse = getK*2 //math.min(rank(singleDomainSimilarity._2)-1, getK*2)

      if (!new File(getInputFileName(List[String](domain))).exists()) {
        val singleDomainSimilarity = calculateSimilarityMatrix(getKnowledgeBase, domainElements, List[String](domain))
        prepareInputFile(getInputFileName(List[String](domain)), singleDomainSimilarity._1, singleDomainSimilarity._2)
      }
      val command = getAlgorithm match {
        case "Spectral" => commandSpectralClustering(getInputFileName(List[String](domain)), getClustersFilename(List[String](domain)), kToUse)
        case "Agglomerative" => commandAgglomerativeClustering(getInputFileName(List[String](domain)), getClustersFilename(List[String](domain)), kToUse)
        case "Affinity" => commandAffinityPropagation(getInputFileName(List[String](domain)), getClustersFilename(List[String](domain)), 0.5)
      }

      cluster(command, getClustersFilename(List[String](domain))).map(x => {
        Random.shuffle(x).slice(0, math.min(x.size, math.max(5, (proportion * x.size).toInt)))
      }).reduce(_ ++ _)
    }
    else {
      domainElements.toSet
    }
  }

  private def prepareInputFile(name: String, domainElements: List[List[String]], similarityMatrix: DenseMatrix[Double]) = {
    println("CLUSTERING Preparing input file [" + Calendar.getInstance().getTime + "]" )
    val writer = new BufferedWriter(new FileWriter(name))
    try {
      writer.write("#" + domainElements.map(_.mkString(":")).mkString(";") + "\n") //print element names

      //matrix content
      for (rowI <- 0 until similarityMatrix.rows) {
        for (columnI <- 0 until similarityMatrix.cols) {
          writer.write(similarityMatrix(rowI, columnI) + "")
          if (columnI < (similarityMatrix.cols - 1)) {
            writer.write(";")
          }
        }
        writer.write("\n")
      }
      println("CLUSTERING Input file prepared [" + Calendar.getInstance().getTime + "]")
    }
    finally {
      writer.close()
    }
  }

  private def clusterQuery(domains: List[String], subSample: Boolean = true, proportion: Double = 0.5) = {
    println(s"CLUSTERING: Clustering query $domains [" +  Calendar.getInstance().getTime + "]")

    if (!new File(getInputFileName(domains)).exists()) {
      val similarity = getSimilarity(domains, subSample, proportion)
      prepareInputFile(getInputFileName(domains), similarity._1, similarity._2)
    }
    println("CLUSTERING: similarity matrix prepared [" + Calendar.getInstance().getTime + "]")

    val kToUse = getK //math.min(rank(similarity._2) - 1, getK)

    val logWrite = new FileWriter(logFile, true)
    logWrite.write("CLUSTERING INITIALIZATION: running command " + commandSpectralClustering(getInputFileName(domains), getClustersFilename(domains), kToUse) + "\n")
    logWrite.close()

    val command = getAlgorithm match {
      case "Spectral" => commandSpectralClustering(getInputFileName(domains), getClustersFilename(domains), kToUse)
      case "Agglomerative" => commandAgglomerativeClustering(getInputFileName(domains), getClustersFilename(domains), kToUse)
      case "Affinity" => commandAffinityPropagation(getInputFileName(domains), getClustersFilename(domains), 0.5)
    }
    try {
      println("CLUSTERING: Starting clustering for the final query [" + Calendar.getInstance().getTime + "]")
      println(s"CLUSTERING: running command -> $command  [" + Calendar.getInstance().getTime + "]")
      cluster(command, getClustersFilename(domains)).toList
    }
    finally {
      println("CLUSTERING: clustering done! ["  + Calendar.getInstance().getTime + "]")
    }
  }

  def getAssignment(domains: List[String], hiddenName: String, subSample: Boolean = true, proportion: Double = 0.5) = {
    if (!getCreatedClusters.contains(domains.mkString(","))) {
      clusterQuery(domains, subSample, proportion).foreach( clust => addCreatedCluster(domains.mkString(","), clust.map( _.mkString(",")).toList))
    }
    println("CLUSTERING: accessing next cluster" + Calendar.getInstance().getTime + "]")
    getNextCluster(domains.mkString(",")).map( x => s"$hiddenName($x)")
  }

  def getAllClusters(domains: List[String], subSample: Boolean = true, proportion: Double = 0.5) = {
    if (!getCreatedClusters.contains(domains.mkString(","))) {
      clusterQuery(domains, subSample, proportion).foreach( clust => addCreatedCluster(domains.mkString(","), clust.map( _.mkString(",")).toList))
    }
    createdClusters(domains.mkString(","))
  }





  //CALCULATING SIMILARITIES

  private def calculateSimilarityMatrix(kBase: KnowledgeBase, domainElements: List[List[String]], domains: List[String]) = {
    val similarityMatrices = Array.ofDim[DenseMatrix[Double]](5).map(x => DenseMatrix.zeros[Double](domainElements.size, domainElements.size))
    val biggerThanZero = Array.ofDim[Int](5).map( x => 0)
    var check = true

    similarityMatrices(0) = fastAttributeSimilarity(domains, domainElements)
    similarityMatrices(2) = fastSameRelationSimilarity(domains, domainElements)
    similarityMatrices(1) = fastIntraConnectionSimilarity(domains, domainElements)

    if (max(similarityMatrices(0)) > 0) { biggerThanZero(0) = 1}
    if (max(similarityMatrices(2)) > 0) { biggerThanZero(2) = 1}
    if (max(similarityMatrices(1)) > 0) { biggerThanZero(1) = 1}
    /*val finalMatrix = DenseMatrix.zeros[Double](domainElements.size, domainElements.size)

    val measureFunctions = List[(List[String], List[String], List[String], KnowledgeBase, (Boolean, Boolean) => Boolean) => Double](calculateAttributeMeasure, calculateConnections, calculateSameRelations)

    for (mFunc <- measureFunctions) {

    }*/

    for (x <- domainElements.indices; y <- (x + 1) until domainElements.size) {
      // uses minus - the lower the number, the examples are more similar (but measures mean the bigger the number, the examples are more similar)
      //val attributeSimilarity = calculateAttributeMeasure(domainElements(x), domainElements(y), domains)
      val connectionSimilarity = calculateConnections(domainElements(x), domainElements(y), domains)
      //val relationSimilarity = calculateSameRelations(domainElements(x), domainElements(y), domains)
      val indirectSimilarity = calculateIndirectLinks(domainElements(x), domainElements(y), domains)
      val attributeNeighbourhood = attributeNeighbourhoodSimilarity(domainElements(x), domainElements(y), domains)

      //if (check && biggerThanZero(0) == 0 && attributeSimilarity > 0) { biggerThanZero(0) = 1 }
      if (check && biggerThanZero(1) == 0 && connectionSimilarity > 0) { biggerThanZero(1) = 1 }
      //if (check && biggerThanZero(2) == 0 && relationSimilarity > 0) { biggerThanZero(2) = 1 }
      if (check && biggerThanZero(3) == 0 && indirectSimilarity > 0) { biggerThanZero(3) = 1 }
      if (check && biggerThanZero(4) == 0 && attributeNeighbourhood > 0) { biggerThanZero(4) = 1 }

      if ( biggerThanZero.sum == 5) { check = false }

      //similarityMatrices(0)(x, y) = attributeSimilarity
      //similarityMatrices(0)(y, x) = attributeSimilarity

      similarityMatrices(1)(x, y) = connectionSimilarity
      similarityMatrices(1)(y, x) = connectionSimilarity

      //similarityMatrices(2)(x, y) = relationSimilarity
      //similarityMatrices(2)(y, x) = relationSimilarity

      similarityMatrices(3)(x, y) = indirectSimilarity
      similarityMatrices(3)(y, x) = indirectSimilarity

      similarityMatrices(4)(x, y) = attributeNeighbourhood
      similarityMatrices(4)(y, x) = attributeNeighbourhood

    }

    //if histogram is used to measure similarity, we get a distance metric (0 meaning very similar) ==> it should be converted to similarity (1 meaning very similar)
    if (getOverlapMeasure == "histogram" && max(similarityMatrices(3)) > 0.0 ) {
      similarityMatrices(3) = DenseMatrix.tabulate(domainElements.size, domainElements.size){case x => 1.0} - normalizeMatrix(similarityMatrices(3))
    }
    if (getOverlapMeasure == "histogram" && max(similarityMatrices(4)) > 0.0 ) {
      similarityMatrices(4) = DenseMatrix.tabulate(domainElements.size, domainElements.size){case x => 1.0} - normalizeMatrix(similarityMatrices(4))
    }

    (domainElements, (getNormalization match {
      case true => similarityMatrices.zipWithIndex.map( x => if (biggerThanZero(x._2) > 0) { normalizeMatrix(x._1) } else { x._1 });
      case false => similarityMatrices
    }).zipWithIndex.map( matrix => matrix._1 :* DenseMatrix.tabulate(matrix._1.rows, matrix._1.cols){case x => scaleFactors(matrix._2)}).reduce(_ + _))
  }

  private def findSimilarTuplesAttribute(domains: List[String], predicate: Predicate) = {
    val reducedDomainsToCombine = domains.map( dom => {
      if (predicate.getDomains.contains(dom)) { predicate.getTrueGroundings }
      else { getKnowledgeBase.getDomain(dom).getElementsAsList}
    })

    reducedDomainsToCombine.reduceLeft((x, y) => { for {xs <- x; ys <- y} yield xs ::: ys }).filter( x => x.toSet.size == domains.length ).map( x => { if ( domains.toSet.size == domains.length) {x} else {x.sorted}}).toList
  }

  def fastAttributeSimilarity(domains: List[String], domainElements: List[List[String]]) = {
    val attributeSimilarityMatrix = DenseMatrix.zeros[Double](domainElements.size, domainElements.size)
    val elementIndices = collection.mutable.Map[List[String], Int]()
    domainElements.zipWithIndex.foreach( x => elementIndices(x._1) = x._2)

    getKnowledgeBase.getPredicateNames.map( getKnowledgeBase.getPredicate ).filter( _.arity == 1).foreach( attribute => {
      val similarElements = findSimilarTuplesAttribute(domains, attribute).filter( elementIndices.contains )


      for( ind1 <- similarElements.indices; ind2 <- ind1 + 1 until similarElements.size) {
        attributeSimilarityMatrix(elementIndices(similarElements(ind1)), elementIndices(similarElements(ind2))) += 1
        attributeSimilarityMatrix(elementIndices(similarElements(ind2)), elementIndices(similarElements(ind1))) += 1
      }

      val similarSet = similarElements.toSet
      val similarByNotHavingAttribute = domainElements.filterNot( similarSet.contains )
      for (ind1 <- similarByNotHavingAttribute.indices; ind2 <- ind1 + 1 until similarByNotHavingAttribute.size) {
        attributeSimilarityMatrix(elementIndices(similarByNotHavingAttribute(ind1)), elementIndices(similarByNotHavingAttribute(ind2))) += 1
        attributeSimilarityMatrix(elementIndices(similarByNotHavingAttribute(ind2)), elementIndices(similarByNotHavingAttribute(ind1))) += 1
      }
    })

    attributeSimilarityMatrix
  }

  private def findSimilarTuplesSameRelation(domains: List[String], predicate: Predicate, focusArgument: Int) = {
    val reducedDomainsToCombine = domains.map( dom => {
      if (predicate.getDomains(focusArgument) == dom) { predicate.getTrueGroundings.map( _.slice(focusArgument, focusArgument + 1)) }
      else { getKnowledgeBase.getDomain(dom).getElementsAsList}
    })

    reducedDomainsToCombine.reduceLeft((x, y) => { for {xs <- x; ys <- y} yield xs ::: ys }).toList
  }

  def fastSameRelationSimilarity(domains: List[String], domainElements: List[List[String]]) = {
    val similarityMatrix = DenseMatrix.zeros[Double](domainElements.size, domainElements.size)
    val elementIndices = collection.mutable.Map[List[String], Int]()
    domainElements.zipWithIndex.foreach( x => elementIndices(x._1) = x._2)

    getKnowledgeBase.getPredicateNames.map( getKnowledgeBase.getPredicate ).filter( _.arity > 1).foreach( predicate => {
      (0 until predicate.arity).foreach( argument => {
        val similarElements = findSimilarTuplesSameRelation(domains, predicate, argument).filter( elementIndices.contains )

        for (ind1 <- similarElements.indices; ind2 <- ind1 + 1 until similarElements.size) {
          similarityMatrix(elementIndices(similarElements(ind1)), elementIndices(similarElements(ind2))) += 1
          similarityMatrix(elementIndices(similarElements(ind2)), elementIndices(similarElements(ind1))) += 1
        }
      })
    })

    similarityMatrix
  }

  def findSimilarTuplesIntraConnections(domains: List[String], predicate: Predicate) = {
    val possibleLink = domains.distinct.map( x => domains.count( _ == x ) <= predicate.getDomains.count( _ == x) ).reduce( _ && _)

    if (possibleLink && domains.size > 1) {
      val resultTuples = collection.mutable.Set[List[String]]()
      val predicateDomains = predicate.getDomains

      predicate.getTrueGroundings.foreach( ground => {
        ground.zip(predicateDomains).filterNot( x => domains.contains(x._2)).combinations(domains.size).foreach( elementCombination => {
          elementCombination.permutations.foreach( elementPermutation => {
            if (elementPermutation.map( _._2) == domains) { resultTuples += elementPermutation.map( _._1) }
          })
        })
      })

      resultTuples.toList
    }
    else { List[List[String]]() }
  }

  def fastIntraConnectionSimilarity(domains: List[String], domainElements: List[List[String]]) = {
    val similarityMatrix = DenseMatrix.zeros[Double](domainElements.size, domainElements.size)
    val elementIndices = collection.mutable.Map[List[String], Int]()
    domainElements.zipWithIndex.foreach( x => elementIndices(x._1) = x._2)

    getKnowledgeBase.getPredicateNames.map( getKnowledgeBase.getPredicate ).filter( _.arity > 1).foreach( predicate => {
      val similarElements = findSimilarTuplesIntraConnections(domains, predicate).filter(elementIndices.contains )

      for (ind1 <- similarElements.indices; ind2 <- ind1 + 1 until similarElements.size) {
        similarityMatrix(elementIndices(similarElements(ind1)), elementIndices(similarElements(ind2))) += 1
        similarityMatrix(elementIndices(similarElements(ind2)), elementIndices(similarElements(ind1))) += 1
      }
    })

    similarityMatrix
  }



  /*
     Given a tuples t1 = (elements1) and t2 = (elements2), calculates a number of attributes (predicate.arity == 1) that tuples share
     :argument [comparator]: controls how sharing is defined when tuple have more than one element

     Example:
         if criteria == && (AND) : all tuple elements matching the domain of a predicate have to evaluate to true
         if criteria == || (OR)  : at least one tuple element matching the domain of a predicate have to evaluate to true

   */
  def calculateAttributeMeasure(tuple1: List[String], tuple2: List[String], domains: List[String], criteria: (Boolean, Boolean) => Boolean = {_ && _}) = {
    val attributePredicates = getKnowledgeBase.getPredicateNames.map(getKnowledgeBase.getPredicate).filter(_.arity == 1)

    // for each attribute predicate
    attributePredicates.filter( x => domains.contains(x.getDomains.head) ).foldLeft[Int](0)( (acc, pred) => {
      val predGroundings = pred.getTrueGroundings
      val firstTuple = tuple1.zipWithIndex.filter( x => domains(x._2) == pred.getDomains.head).map( x => predGroundings.contains(List(x._1))).reduce(criteria(_, _))
      val secondTuple = tuple2.zipWithIndex.filter( x => domains(x._2) == pred.getDomains.head).map( x => predGroundings.contains(List(x._1))).reduce(criteria(_, _))

      if (firstTuple == secondTuple) { acc + 1 }
      else { acc }
    }).toDouble
  }

  /*
    Calculates the number of connection between objects in the same tuple
   */
  private def intraTupleConnections(tuple1: List[String], tuple2: List[String], domains: List[String]) = {
    getKnowledgeBase.getPredicateNames.map( getKnowledgeBase.getPredicate ).filter( _.arity > 1).filter( x => x.getDomains == domains || x.getDomains == domains.reverse).foldLeft(0)( (acc, pred) => {
      val firstTrue = pred.getTrueGroundings.contains(tuple1) || pred.getTrueGroundings.contains(tuple1.reverse)
      val secondTrue = pred.getTrueGroundings.contains(tuple2) || pred.getTrueGroundings.contains(tuple2.reverse)

      if (firstTrue && secondTrue) { acc + 1 }
      else { acc }
    }).toDouble
  }

  /*
    Calculates the number of connections between elements of tuples t1 and t2
     [Connection can only go from one tuple to the other one]
   */
  private def interTupleConnections(tuple1: List[String], tuple2: List[String], domains: List[String]) = {
    tuple1.indices.foldLeft(0)( (acc, elemInd) => {
      val graph = getNeighbourhoodGraph(tuple1(elemInd), domains(elemInd), 0).collectTypeInformation() //new NeighbourhoodGraph(tuple1(elemInd), domains(elemInd), 0, getKnowledgeBase).collectTypeInformation()

      acc + tuple2.zipWithIndex.filter( _._2 >= elemInd).foldLeft(0)( (acc_i, tupleElem) => {
        if (graph(0).contains(domains(tupleElem._2)) && graph(0)(domains(tupleElem._2)).contains(tupleElem._1)) { acc_i + 1 }
        else { acc_i }
      })

    })
  }

  /*
    Given two tuples t1 and t2, calculates the number of connection that can be made between objects of tuples t1 and t2
   */
  private def calculateConnections(tuple1: List[String], tuple2: List[String], domains: List[String], combination: (Boolean, Boolean) => Boolean = {(x,y) => true}) = {

    //over tuple elements
    interTupleConnections(tuple1, tuple2, domains) //+ intraTupleConnections(tuple1, tuple2, domains)
  }

  /*
    Calculates the number of relations tuples have in common (argument- and element-wise)
   */
  private def calculateSameRelations(tuple1: List[String], tuple2: List[String], domains: List[String], criteria: (Boolean, Boolean) => Boolean = { _ && _}) = {
    val predicates = getKnowledgeBase.getPredicateNames.map(getKnowledgeBase.getPredicate).filter(_.arity > 1)

    //for each relation predicate
    predicates.foldLeft(0)( (acc, pred) => {
      //for each argument
      acc + pred.getDomains.zipWithIndex.filter( x => domains.contains(x._1) ).foldLeft(0)( (acc_i, focusArgument) => {
        val trueGroundings = pred.getTrueGroundings.map( x => x(focusArgument._2)) // select only the position of interest
        val firstTrue = tuple1.zipWithIndex.filter( x => domains(x._2) == focusArgument._1).map( _._1 ).map( x => trueGroundings.contains(x)).reduce( criteria(_, _))
        val secondTrue = tuple2.zipWithIndex.filter( x => domains(x._2) == focusArgument._1).map( _._1 ).map( x => trueGroundings.contains(x)).reduce( criteria(_, _))

        if ( firstTrue && secondTrue) { acc_i + 1 }
        else { acc_i }
      })
    }).toDouble
  }

  private def graphNeighbourhoodIntersection(first: collection.mutable.Map[Int,collection.mutable.Map[String,List[String]]],
                                             second: collection.mutable.Map[Int,collection.mutable.Map[String,List[String]]]) = {

    val resultIntersection = collection.mutable.Map[Int,collection.mutable.Map[String,List[String]]]()
    //go over levels
    first.keys.foreach( level => {
      resultIntersection(level) = collection.mutable.Map[String,List[String]]()
      //go over domains
      (first(level).keySet ++ second(level).keySet).foreach( domain => {

        val intersectionOverDomains = first(level).getOrElse(domain, List[String]()).intersect(second(level).getOrElse(domain, List[String]()))
        if (intersectionOverDomains.nonEmpty) {
          resultIntersection(level)(domain) = intersectionOverDomains
        }
      })
    })

    resultIntersection
  }

  /*
    Calculates the amount of indirect connections between two tuples (referring to the same thing over multiple relations chained)
   */
  def calculateIndirectLinks(tuple1: List[String], tuple2: List[String], domains: List[String], combination: (Boolean, Boolean) => Boolean = {(x,y) => true}) = {
    //going over different domains
    domains.toSet.foldLeft(0.0)( (acc, dom) => {
      //                                                                               new NeighbourhoodGraph(x._1, domains(x._2), getJumpStep, getKnowledgeBase)
      val firstGraph = tuple1.zipWithIndex.filter( x => domains(x._2) == dom).map(x => getNeighbourhoodGraph(x._1, domains(x._2), getJumpStep).collectTypeInformation()).reduce(graphNeighbourhoodIntersection)
      val secondGraph = tuple2.zipWithIndex.filter( x => domains(x._2) == dom).map(x => getNeighbourhoodGraph(x._1, domains(x._2), getJumpStep).collectTypeInformation()).reduce(graphNeighbourhoodIntersection)

      //go over levels
      if (getOverlapMeasure != "histogram") {
        val intersect = graphNeighbourhoodIntersection(firstGraph, secondGraph)
        acc + intersect.keys.foldLeft(0.0)((acc_i, level) => {
          acc_i + intersect(level).keys.foldLeft(0.0)((acc_ii, dom) => {
            acc_ii + (getOverlapMeasure match {
              case "union" => intersect(level)(dom).toSet.size.toDouble / firstGraph(level)(dom).toSet.union(secondGraph(level)(dom).toSet).size
              case "min" => intersect(level)(dom).toSet.size.toDouble / math.min(firstGraph(level)(dom).toSet.size, secondGraph(level)(dom).toSet.size)
              case "max" => intersect(level)(dom).toSet.size.toDouble / math.max(firstGraph(level)(dom).toSet.size, secondGraph(level)(dom).toSet.size)
            })
          })
        })
      }
      else {
        acc + (0 to getJumpStep).foldLeft(0.0)( (acc_i, level) => {
          acc_i + (firstGraph(level).keySet ++ secondGraph(level).keySet).foldLeft(0.0)( (acc_ii, dom) => {
            val histograms = Histogram.create(firstGraph(level).getOrElse(dom, List[String]()), secondGraph(level).getOrElse(dom, List[String]()))
            acc_ii + math.abs(HistogramDistance.chiSquared(histograms._1, histograms._2))
          })
        })
      }
    })
  }

  def attributeNeighbourhoodSimilarity(tuple1: List[String], tuple2: List[String], domains: List[String]) = {

    val firstGraphs = tuple1.zipWithIndex.map( elem => getAnyNeighbourhoodGraph(elem._1, domains(elem._2)).collectTypeInformation(0))
    val secondGraphs = tuple2.zipWithIndex.map( elem => getAnyNeighbourhoodGraph(elem._1, domains(elem._2)).collectTypeInformation(0))

    val firstAttributes = firstGraphs.zipWithIndex.map( x => x._1.keys.foldLeft(List[String]())( (acc, dom) => {
      acc ::: x._1(dom).foldLeft(List[String]())( (acc_i, elem) => acc_i ::: getAttributes("", elem))
    }) ).reduce( _ ::: _ )

    val secondAttributes = secondGraphs.zipWithIndex.map( x => x._1.keys.foldLeft(List[String]())( (acc, dom) => {
      acc ::: x._1(dom).foldLeft(List[String]())( (acc_i, elem) => acc_i ::: getAttributes("", elem))
    }) ).reduce( _ ::: _ )

    if (firstAttributes.isEmpty && secondAttributes.isEmpty) { 0.0 }
    else {
      getOverlapMeasure match {
        case "min" =>
          val (firstSet, secondSet) = (firstAttributes.toSet, secondAttributes.toSet)
          firstSet.intersect(secondSet).size.toDouble / math.min(firstSet.size, secondSet.size)
        case "max" =>
          val (firstSet, secondSet) = (firstAttributes.toSet, secondAttributes.toSet)
          firstSet.intersect(secondSet).size.toDouble / math.max(firstSet.size, secondSet.size)
        case "union" =>
          val (firstSet, secondSet) = (firstAttributes.toSet, secondAttributes.toSet)
          firstSet.intersect(secondSet).size.toDouble / firstSet.union(secondSet).size
        case "histogram" =>
          val histograms = Histogram.create(firstAttributes, secondAttributes)
          math.abs(HistogramDistance.chiSquared(histograms._1, histograms._2))
      }
    }
  }

  /*
    converts a similarity matrix to a distance matrix
      (first normalizes the similarity matrix, multiplies it with -1 to revert the meaning of the numbers (smaller number, closer distance),
              and translates it to 0 -  meaning zero distance between examples )
   */
  def similarityToDistance(matrix: DenseMatrix[Double]) = {
    DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => 1.0 } :- normalizeMatrix(matrix)
  }

  /*
    Normalize matrix by using its biggest element as a normalizing constant
   */
  def normalizeMatrix(matrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    val minValue = min(matrix)
    val matrixToUse = minValue < 0.0 match {
      case true => matrix - DenseMatrix.tabulate(matrix.rows, matrix.cols){ case x => minValue }
      case false => matrix
    }
    val normConstant = math.abs(max(matrixToUse))
    matrixToUse :/ DenseMatrix.tabulate(matrix.rows, matrix.cols) { case x => normConstant }
  }





  // CLUSTERING COMMANDS GENERATION

  private def commandAffinityPropagation(inputFile: String, output: String, damping: Double, preference: Double) = {
    s"python $getClusteringFile --alg Affinity --input $inputFile --output $output --damping $damping --pref $preference"
  }

  private def commandAffinityPropagation(inputFile: String, output: String, damping: Double) = {
    s"python $getClusteringFile --alg Affinity --input $inputFile --output $output --damping $damping"
  }

  private def commandSpectralClustering(inputFile: String, output: String, k: Int) = {
    s"python $getClusteringFile --alg Spectral --input $inputFile --output $output --k $k"
  }

  private def commandAgglomerativeClustering(inputFile: String, output: String, k: Int) = {
    s"python $getClusteringFile --alg Agglomerative --input $inputFile --output $output --k $k"
  }

  private def cluster(command: String, clustersFile: String) = {
    val logInner = new FileWriter(getLogFile, true)
    val log = new BufferedWriter(logInner)
    command.!(ProcessLogger(line => {}, line => {log.write("CLUSTERING ERROR: " + line + "   [" + Calendar.getInstance().getTime + "]" + "\n")}))
    println("CLUSTERING clustering done! [" + Calendar.getInstance().getTime + "]")
    log.close()
    logInner.close()

    var clusters = collection.mutable.Set[List[List[String]]]()

    Helper.readFile(clustersFile).foreach( line => {
      clusters = clusters + line.split("""\{""")(1).replace("}", "").split(";").toList.map( _.split(":").toList)
    })

    clusters.map( _.toSet)
  }
}
