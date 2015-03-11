package edu.arizona.sista.odin.export.biopax

import java.io._

import scala.collection.mutable.MutableList
import scala.collection.mutable.Map

import edu.arizona.sista.processors._
import edu.arizona.sista.odin._

import org.biopax.paxtools.io._
import org.biopax.paxtools.model._
import org.biopax.paxtools.model.level3._

/**
  * Defines implicit classes used to build and output BioPax models.
  *   Written by Tom Hicks. 3/6/2015.
  *   Last Modified: Move into Processor library.
  */
class BioPaxer {
  val SistaBaseUrl = "http://nlp.sista.arizona.edu/reach"
  val SistaDefaultCharset = "UTF-8"
  // val UniprotBaseUrl = "http://identifiers.org/uniprot" // REAL Uniprot base
  // val UniprotDb = "uniprotkb"               // REAL Uniprot knowledge base
  val UniprotDb = "fake-uniprotkb"
  val UniprotBaseUrl = "http://nlp.sista.arizona.edu/fake-uniprot"

  // incrementing counter for numbering entities
  protected var idCntr = 0
  def currentId():String = { s"${idCntr}" }
  def genNextId():String = {
    idCntr = idCntr + 1
    return currentId()
  }

  // map of protein type names to protein reference information
  val proteinRefs:scala.collection.mutable.Map[String, ProteinRef] =
    scala.collection.mutable.Map[String, ProteinRef]()


  /** Build and return a BioPax model for the given sequence of mentions. */
  def buildModel (mentions:Seq[Mention], doc:Document): Model = {
    val factory: BioPAXFactory = BioPAXLevel.L3.getDefaultFactory()
    var model:Model = factory.createModel()
    model.setXmlBase(SistaBaseUrl)
    mentions.foreach { addMention(model, _) }
    return model
  }

  /** Add the given mention to the given model and return the model. */
  def addMention (model:Model, mention: Mention): Model = {
    mention match {
      case mention: TextBoundMention =>
        doTextMention(model, mention)
      case mention: EventMention =>
        doEventMention(model, mention)
      case mention: RelationMention =>
        doRelationMention(model, mention)
      case _ => ()
    }
    return model                            // return updated model
  }

  def doEventMention (model:Model, m:Mention) = {
  }

  def doRelationMention (model:Model, m:Mention) = {
  }

  def doTextMention (model:Model, mention:Mention) = {
    val tmType:String = mention.label
    val tmName:String = mention.text
    tmType match {
      case "Cellular_component" =>
      case "Gene_or_gene_product" =>
      case "Protein" => handleProtein(model, mention, tmName)
      case "Simple_chemical" =>
      case "Site" =>
      case _ => ()
    }
  }

  def genProteinUrl(id:String):String = {
    return s"${UniprotBaseUrl}/${id}"
  }

  def genXrefUrl(id:String):String = {
    return s"${SistaBaseUrl}#xref_${id}"
  }

  /** Handle the given Protein wrt the given Model. */
  def handleProtein (model:Model, mention:Mention, pName:String) = {
    if (!proteinRefs.contains(pName)) {     // if this is a new protein
      val pRef:ProteinRef = registerProtein(pName) // register the new protein
      addProteinToModel(model, pRef)        // and add it to the model
    }
  }

  def addProteinToModel (model:Model, pRef:ProteinRef) = {
    val uxref:UnificationXref = model.addNew(classOf[UnificationXref], genXrefUrl(pRef.id))
    uxref.setDb(UniprotDb);
    uxref.setId(pRef.id);
    val prf:ProteinReference = model.addNew(classOf[ProteinReference], pRef.url)
    prf.setDisplayName(pRef.displayName);
    prf.addXref(uxref);
  }

  /** Return the given model as a single BioPax OWL string. */
  def modelToString (model:Model): String = {
    val bpIOH:BioPAXIOHandler = new SimpleIOHandler()
    val baos:ByteArrayOutputStream = new ByteArrayOutputStream()
    bpIOH.convertToOWL(model, baos)
    return baos.toString(SistaDefaultCharset)
  }

  /** Output the given model to the given output stream. */
  def outputModel (model:Model, out:OutputStream): Unit = {
    val bpIOH:BioPAXIOHandler = new SimpleIOHandler()
    bpIOH.convertToOWL(model, out)
  }

  def registerProtein (pName:String): ProteinRef = {
    val nid:String = genNextId()
    val pRef:ProteinRef = new ProteinRef(nid, pName, pName, genProteinUrl(nid))
    proteinRefs.put(pName, pRef)
    return pRef
  }


  /** Generates a BioPax representation of the given mention as a list of strings. */
  def mentionToStrings (mention: Mention): List[String] = {
    val mStrings:MutableList[String] = MutableList[String]()
    mention match {
      case mention: TextBoundMention =>
        mStrings += s"TextMention: ${mention.label}"
        mStrings += s"text: ${mention.text}"
        mStrings += ("=" * 72)
      case mention: EventMention =>
        mStrings += s"EventMention: ${mention.label}"
        mStrings += s"text: ${mention.text}"
        mStrings += s"trigger: ${mention.trigger.text}"
        mention.arguments foreach {
          case (k,vs) => for (v <- vs) mStrings += s"$k = ${v.text}"
        }
        mStrings += ("=" * 72)
      case mention: RelationMention =>
        mStrings += s"RelationMention: ${mention.label}"
        mStrings += s"text: ${mention.text}"
        mention.arguments foreach {
          case (k,vs) => for (v <- vs) mStrings += s"$k = ${v.text}"
        }
        mStrings += ("=" * 72)
      case _ => ()
    }
    return mStrings.toList
  }

}


/** Class for a Protein reference. */
class ProteinRef (
  val id:String,
  val displayName:String,
  val name:String,
  val url:String
)
