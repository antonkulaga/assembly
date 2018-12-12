package assembly.cloning

import assembly.cloning
import assembly.extensions._

import scala.io.Source

/**
  * Class to work with restriction enzymes
  * @param enzymes2sites
  */
case class RestrictionEnzymes(enzymes2sites: Set[(String, String)]) {


  lazy val enzymes2sitesMap: Map[String, String] = enzymes2sites.toMap

  lazy val sites2enzymes: Set[(String, String)] = enzymes2sites.map(_.swap)

  lazy val seq2enzymesMap: Map[String, Set[String]] = sites2enzymes.groupBy(_._1).mapValues(s=>s.map(_._2))

  /*
  def find(where: String): Set[String] = seq2enzymesMap.flatMap{
    case (str, enzymes)=> if(where.contains(str)) enzymes else Set.empty[String]
  }.toSet
  */

  def find(where: String, reverseComplement: Boolean): Map[(String, String), List[Int]] = {
    val found: Map[(String, String), List[Int]] = enzymes2sites.map{
      case (enzyme, site) => enzyme -> site-> site.inclusionsInto(where)
    }.toMap
    if(reverseComplement) found ++ find(where.reverseComplement, reverseComplement = false) else found
  }

  /**
    * Checks if any of the restriction enzymes cuts the sequence
    * @param where
    * @param reverseComplement
    * @return
    */
  def canCut(where: String, reverseComplement: Boolean): Boolean = {
    enzymes2sites.exists{ case (_, site) => where.contains(site)} && (!reverseComplement || canCut(where.reverseComplement, false))
  }


}

object RestrictionEnzymes {


  val BsaI = RestrictionEnzyme("BsaI" , "GGTCTC", 7, 11)

  lazy val commonEnzymesSet: Set[(String, String)] = CommonEnzymes.common

  val ALL_COMMON_ENZYMES = RestrictionEnzymes(commonEnzymesSet)
  val GOLDEN_GATE_BsaI = RestrictionEnzymes(commonEnzymesSet.filter{ case (e, _)=> e =="BsaI" })
  val GOLDEN_GATE_BsbI = RestrictionEnzymes(commonEnzymesSet.filter{ case (e, _)=> e == "BsbI"})
  val GOLDEN_GATE = RestrictionEnzymes(commonEnzymesSet.filter{ case (e, _)=> e =="BsaI" || e == "BsbI"}) //,  "BspTNI" -> "GGTCTC",  "Bso31I" -> "GGTCTC", "Eco31I" -> "GGTCTC"

  lazy val empty = RestrictionEnzymes(Set.empty[(String, String)])
  def apply(enzymes: (String, String)*): RestrictionEnzymes = RestrictionEnzymes(Set(enzymes:_*))
}