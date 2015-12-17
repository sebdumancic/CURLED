package deepSRL.basics

import java.io.{BufferedWriter, FileWriter}

import deepSRL.utils.Helper
import deepSRL.formats.Format

/**
 *  This class implements a knowledge base managing predicates and their true groundings
 *
 * Created by seb on 08.05.15.
 */


class KnowledgeBase(
                   val databases: Seq[String],
                   val header: String
                     ) {

  private val predicates = collection.mutable.Map[String, Predicate]()
  val predicate_regex = """(.*?)\((.*?)\).*""".r
  private val domainObjects = collection.mutable.Map[String, Domain]()

  constructPredicates()
  processDatabases()

  def getDomainObject(name: String) = {
    if (!domainObjects.contains(name)) { domainObjects(name) = new Domain(name)}
    domainObjects(name)
  }

  def createPredicate(name: String, domains: List[String]) = {
    predicates(name) = new Predicate(name.trim, domains.map( x => getDomainObject(x.trim) ))
  }

  def addPredicate(pred: Predicate) = {
    predicates(pred.getName) = pred
  }

  def getPredicate(name: String) = {
    require(predicates contains name, s"Requesting non-existing predicate $name!")
    predicates(name)
  }

  def getPredicateNames = {
    predicates.keys.toList
  }

  def addToDomain(domain: String, domainObject: String) = {
    getDomainObject(domain.trim).addElement(domainObject)
  }

  def getDomain(domain: String) = {
    require(domainObjects contains domain, s"Accessing a non-existing domain $domain")
    domainObjects(domain)
  }

  def getAllDomains = { domainObjects }

  private def constructPredicates() = {
    for(predicate <- header.split("\n")) {
      if (predicate.length > 2 && !predicate.contains("//")) {
        val predicate_regex(predicate_name, predicate_args) = predicate
        createPredicate(cleanForm(predicate_name), predicate_args.split(",").toList) // cleans predicate name in case it is in the autoencoder form
      }

    }
  }

  def addTrueGrounding(grounding: String) = {
    val predicate_regex(predicate_name, predicate_arguments) = grounding
    val argument_list = predicate_arguments.split(",").toList
    getPredicate(predicate_name).setTrueGrounding(argument_list.map( _.trim ))

    // add an element to the domain
    for (mapping <- argument_list.zip(getPredicate(predicate_name).getDomains)) {
      addToDomain(mapping._2, mapping._1.trim)
    }
  }

  private def cleanForm(line: String) = {
    //in PredicateInvention a model have to be re-instantiated with a new knowledge bases that might be in form of predicateName[_source|_target]
    line.replace("_target", "").replace("_source", "")
  }

  private def processDatabase(db: String) = {
    for(line <- Helper.readFile(db)) {
      if (line.length > 2) {
        addTrueGrounding(cleanForm(line))
      }

    }
  }

  private def processDatabases() = {
    for(db <- databases) {
      processDatabase(db)
    }
  }

  def printToFile(filename: String) = {
    val writer = new BufferedWriter(new FileWriter(filename))

    for(predicate <- predicates.keys) {
      for(grounding <- predicates(predicate).getTrueGroundings) {
        writer.write(s"$predicate(" + grounding.mkString(",") + ").\n")
      }
    }
    writer.close()
  }


  def printToFileWithTarget(filename: String, target: String) = {

    val writer = new BufferedWriter(new FileWriter(filename))

    // generate everything except target predicate
    for(predicate <- getPredicateNames) {
      if (predicate != target) {
        for (grounding <- predicates(predicate).getTrueGroundings) {
          writer.write(predicate.toLowerCase.replace('-','_') + "(" + grounding.mkString(",").toLowerCase + ").\n")
        }
      }
    }

    // add target predicate with truth values
    val target_predicate = getPredicate(target)
    var domains = collection.mutable.Seq[collection.mutable.Set[List[String]]]()

    for (dom <- target_predicate.getDomains) {
      domains = domains :+ getDomain(dom).getElementsAsList
    }

    for (ground <- domains.reduceLeft( (x,y) => { for { xs <-x; ys <- y} yield xs ::: ys} )) {
      if (target_predicate.isTrue(ground)) {
        writer.write(target.toLowerCase.replace('-','_') + "(" + ground.mkString(",").toLowerCase + ",pos).\n" )
      }
      else {
        writer.write(target.toLowerCase.replace('-','_') + "(" + ground.mkString(",").toLowerCase + ",neg).\n" )
      }
    }

    writer.close()
  }

  def printToFile(filename: String, asFormat: String = "MLN") = {
    val writer = new BufferedWriter(new FileWriter(filename))

    // generate everything except target predicate
    try {
      getPredicateNames.map(getPredicate).foreach(predicate => {
        predicate.getTrueGroundings.foreach(ground => writer.write(asFormat match {
          case "tilde" => Format.tildeFormat(predicate.getName, ground.mkString(",")) + "\n"
          case "MLN" => Format.MLNFormatFact(predicate.getName, ground.mkString(",")) + "\n"
        }))
      })
    }
    catch {
      case e: Exception => println(e.getMessage, e.getStackTrace, e.getCause)
    }
    finally {
      writer.close()
    }
  }

}


