package assembly.cloning

import scala.annotation.tailrec
import assembly.extensions._


case class RestrictionEnzyme(name: String, site: String, forwardCut: Int, reverseCut: Int) extends RestrictionEnzymeLike {

}

/**
  * Trait for all enzymes that can cut
  */
trait RestrictionEnzymeLike extends SearchDNA {

  def forwardCut: Int

  def reverseCut: Int

  def stickyLength: Int = Math.abs(forwardCut - reverseCut)

  def forwardGap: Int = forwardCut - site.length
  def reverseGap: Int = reverseCut - site.length

  def site: String

  def name: String

  /**
    * Positions of all recognition sites in a forward strand of a sequence
    * @param sequence
    * @return
    */
  def positionsForward(sequence: String): List[Int] = searchesOf(sequence, site)

  def positionsReverseComplement(sequence: String): List[Int] = {
    positionsForward(sequence.reverseComplement).map{ i => sequence.length - site.length - i}
  }
  def positions(sequence: String): (List[Int], List[Int]) = (positionsForward(sequence), positionsReverseComplement(sequence))

  def cutForward(sequence: String): Seq[(Int, Int)] = positionsForward(sequence).map(v=> (v + forwardCut, v + reverseCut))
  def cutReverseComplement(sequence: String): Seq[(Int, Int)] ={
    positionsReverseComplement(sequence).map(v=> (v - reverseCut + site.length, v - forwardCut + site.length))
  }

  def cuts(sequence: String): (Seq[(Int, Int)], Seq[(Int, Int)]) =  (cutForward(sequence), cutReverseComplement(sequence))
}

trait SearchDNA {



  def compare(what: String, where: String, start: Int): Boolean = what.indices
    .forall{ i=> basesEqual(what(i).toUpper, where(start + i).toUpper) }

  def basesEqual(base1: Char, base2: Char): Boolean = (base1.toUpper, base2.toUpper) match {
    case (a, b) if a == b => true
    case ('N', _) => true
    case ('V', b) => b != 'T'
    //case ('H', b) => b != 'G'
    //case ('D', b) => b != 'C'
    //case ('B', b) => b != 'A'
    case ('W', b) => b == 'A' || b == 'T' //weak bonds
    case ('S', b) => b == 'G' || b == 'C' //strong bonds
    //case ('M', b) => b == 'A' || b == 'C' //amino
    //case ('K', b) => b == 'G' || b == 'T' //keto
    case ('Y', b) => b == 'T' || b == 'C' //pyrimidine
    case ('R', b) => b == 'G' || b == 'A' //purine
    case _ => false
  }

  @tailrec final def matchSeq(what: String)(where: String, start: Int, spacerAfter: Int): Int =
    if(start + what.length + spacerAfter > where.length) -1
    else
    if(compare(what, where, start)) start else matchSeq(what)(where, start + 1, spacerAfter)


  @tailrec final def searchesOf(where: String, what: String, start: Int = 0,
                                spacerBefore: Int = 0,
                                spacerAfter: Int = 0, acc: List[Int] = Nil): List[Int] = {
    if(start < spacerBefore)
      searchesOf(where, what, spacerBefore, spacerBefore, spacerAfter, acc)
    else
    if( start + what.length + spacerAfter > where.length ) acc.reverse
    else
      matchSeq(what)(where, start, spacerAfter) match {
        case -1 => acc.reverse
        case index =>
          searchesOf(where, what, index + 1, spacerBefore, spacerAfter, index :: acc)
      }
  }
}