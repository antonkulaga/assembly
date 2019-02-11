package assembly.cloning
import assembly.extensions._
import assembly.synthesis.StringTemplate
import scala.collection.compat._
/**
  * Optimized for BsaI, did not check for others yet
  * @param enzyme
  */
case class GoldenGate(enzyme: RestrictionEnzyme, nonOverlap: String = "C") extends Assembly {
  require(Math.abs(enzyme.forwardCut) >= enzyme.site.length, "GoldenGate enzymes should cut outside their recognition sequence")

  def makeNonOverlap(): String = nonOverlap.randomize * Math.abs(enzyme.forwardGap)

  /**
    * Adds sites required for GoldenGate assembly of fragments we want to synthesize
    * @param sequences sequences we want to stitch
    * @param stickyLeft left flank
    * @param stickyRight right flank
    * @return
    */
  def synthesize(sequences: Seq[String], stickyLeft: String, stickyRight: String): List[String] = {
    val rev = enzyme.site.reverseComplement
    require({!sequences.exists(s=>s.contains(enzyme.site) || s.contains(rev)) },
      s"sequences to synthesize should not contain restriction site ${enzyme.site} of the ${enzyme.name}")

    def addLeft(seq: String): String = enzyme.site + makeNonOverlap() + seq
    def addRight(one: String, two: String): String = one + two.take(Math.abs(enzyme.stickyLength)) + makeNonOverlap() + rev
    val updated = (sequences :+ "").tail.sliding(2, 1).map{
      case one::""::Nil => addLeft(one) + stickyRight  + makeNonOverlap() + rev
      case one::two::Nil => addLeft(addRight(one, two))
    }.toList
    val first =  enzyme.site + makeNonOverlap() + stickyLeft + addRight(sequences.head, sequences.tail.head)
    first::updated
  }

  protected def tooSimilar(one: String, two: String, minDifference: Int): Boolean =
    (minDifference < 2 &&  one == two) || ( Math.abs(one.length - two.length) + one.zip(two).count{ case (a, b) => a != b } < minDifference)

  /**
    * Checks if it can potentially stick together
    * @param value
    * @param previous
    * @return
    */
  def checkEnds(value: String, previous: List[String], minStickyDifference: Int, minGCbases: Int = 1): Boolean = {
    val leftSticky = value.take(enzyme.stickyLength)
    val rv = leftSticky.reverseComplement
    val noEnzymeSite = !(value.contains(enzyme.site) || value.contains(enzyme.site.reverseComplement))
    val okGC = minGCbases <1 || (leftSticky.count(c => c == 'G' || c == 'C') >= minGCbases)
    //val noSameSticky = previous.forall(s=> !s.startsWith(leftSticky) && !s.endsWith(rv))

    val noSameSticky = if(minStickyDifference == 1) previous.forall(s=> !s.startsWith(leftSticky) && !s.endsWith(rv)) else
      previous.forall(s=> !(tooSimilar(s.take(leftSticky.length), leftSticky, minStickyDifference) || tooSimilar(s.takeRight(rv.length), rv, minStickyDifference)))
    okGC && noEnzymeSite && noSameSticky}

}

trait Assembly {
  def synthesizeNamed(sequences: Seq[(String, String)], stickyLeft: String, stickyRight: String): Seq[(String, String)] = sequences.map(_._1).zip(synthesize(sequences.map(_._2), stickyLeft, stickyRight))
  def synthesize(sequences: Seq[String], stickyLeft: String, stickyRight: String): List[String]

}
