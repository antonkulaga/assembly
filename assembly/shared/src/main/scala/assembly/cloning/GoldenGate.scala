package assembly.cloning
import assembly.extensions._
import assembly.synthesis.StringTemplate

/**
  * Optimized for BsaI, did not check for others yet
  * @param enzyme
  */
case class GoldenGate(enzyme: RestrictionEnzyme, nonOverlap: String = "C") extends Assembly {
  require(Math.abs(enzyme.forwardCut) >= enzyme.site.length, "GoldenGate enzymes should cut outside their recognition sequence")

  def synthesize(sequences: Seq[String], stickyLeft: String, stickyRight: String): List[String] = {
    val rev = enzyme.site.reverseComplement
    require({!sequences.exists(s=>s.contains(enzyme.site) || s.contains(rev)) }, s"sequences to synthesize should not contain restriction site ${enzyme.site} of the ${enzyme.name}")

    def addLeft(seq: String): String = enzyme.site + (nonOverlap.randomize * Math.abs(enzyme.forwardGap)) + seq //"C" to avoid methylation https://international.neb.com/tools-and-resources/usage-guidelines/dam-and-dcm-methylases-of-e-coli
    def addRight(one: String, two: String): String = one + two.take(Math.abs(enzyme.stickyLength)) + (nonOverlap * Math.abs(enzyme.forwardGap)) + rev
    val updated = (sequences :+ "").tail.sliding(2, 1).map{
      case one::""::Nil => addLeft(one) + stickyRight  + (nonOverlap.randomize * Math.abs(enzyme.forwardGap)) + rev
      case one::two::Nil => addLeft(addRight(one, two))
    }.toList
    val first =  enzyme.site + (nonOverlap.randomize * Math.abs(enzyme.forwardGap)) + stickyLeft + addRight(sequences.head, sequences.tail.head)
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
  def checkEnds(value: String, previous: List[String], minStickyDifference: Int = 2, minGCbases: Int = 1): Boolean = {
    val leftSticky = value.take(enzyme.stickyLength)
    val rv = leftSticky.reverseComplement
    val noEnzymeSite = !(value.contains(enzyme.site) || value.contains(enzyme.site.reverseComplement))
    val okGC = minGCbases <1 || (leftSticky.count(c => c == 'G' || c == 'C') >= minGCbases)
    //val noSameSticky = previous.forall(s=> !s.startsWith(leftSticky) && !s.endsWith(rv))
    val noSameSticky = previous.forall(s=> !
      (tooSimilar(s.take(leftSticky.length), leftSticky, minStickyDifference) || tooSimilar(s.takeRight(rv.length), rv, minStickyDifference))
    )
    okGC && noEnzymeSite && noSameSticky}

}

trait Assembly {
  def synthesizeNamed(sequences: Seq[(String, String)], stickyLeft: String, stickyRight: String): Seq[(String, String)] = sequences.map(_._1).zip(synthesize(sequences.map(_._2), stickyLeft, stickyRight))
  def synthesize(sequences: Seq[String], stickyLeft: String, stickyRight: String): List[String]

}
