package assembly

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.util.Random

package object extensions
{


  /**
    * Nucleotide code-etters
    */
  val nucs: Map[Char, Array[Char]] = Map(
    'W' -> Array('A', 'T'), //weak
    'S' -> Array('C', 'G'), //strong
    'M' -> Array('A', 'C'), //amino
    'K' -> Array('G', 'T'), //keto
    'R' -> Array('A', 'G'), //purine
    'Y' -> Array('C', 'T'), //pyramidine
    'B' -> Array('C', 'G', 'T'), //not A
    'D' -> Array('A', 'G', 'T'), //not C
    'H' -> Array('A', 'C', 'T'), //not G
    'V' -> Array('A', 'C', 'G'), //not T
    'N' -> Array('A', 'C', 'G', 'T') //any
  )

  protected def arr[T](items:Array[T]):T = {
    items(Random.nextInt(items.length))
  }


  implicit def convertNucleotide(char: Char): Nucleotide = new Nucleotide(char)

  class Nucleotide(val char: Char) extends AnyVal {

    def getRandom(mp: Map[Char, Array[Char]]): Char = mp.get(char) match {
      case Some(a) => arr(a)
      case None => char
    }

    def complement: Char = char.toUpper match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
      case '-' => '-' //gap
      case other => other //not sure if I should throw here
    }

    def randomize: Char = getRandom(nucs)

  }


  implicit def convertStringSequence(str: String): StringSeq = new StringSeq(str)

  def basesEqual(base1: Char, base2: Char): Boolean = (base1, base2) match {
    case (a, b) if a == b => true
    case ('N', _) => true
    case ('V', b) => b != 'T'
    case ('H', b) => b != 'G'
    case ('D', b) => b != 'C'
    case ('B', b) => b != 'A'
    case ('W', b) => b == 'A' || b == 'T' //weak bonds
    case ('S', b) => b == 'G' || b == 'C' //strong bonds
    case ('M', b) => b == 'A' || b == 'C' //amino
    case ('K', b) => b == 'G' || b == 'T' //keto
    case ('Y', b) => b == 'T' || b == 'C' //pyrimidine
    case ('R', b) => b == 'G' || b == 'A' //purine
    case _ => false
  }

  def compareSeq(what: String, where: String, start: Int): Boolean = what.indices
    .forall{ i=> basesEqual(what(i).toUpper, where(start + i).toUpper) }

  def seqsInclusionsInto(what: String, where: String): List[Int] = {
    what.matchesIn(where, 0)(compareSeq)
  }


  /*  *
    * Implicit class that extends string with sequences with extra methods
    * @param str
    */
  class StringSeq(val string: String) extends AnyVal {


    def complement: String = string.toUpperCase.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
      case other => other //not sure if I should throw here
    }

    def reverseComplement: String = new StringSeq(string.reverse).complement


    def randomize: String = {
      string.toUpperCase.map(s=>convertNucleotide(s).randomize)
    }

    @tailrec final def inclusionsInto(where: String, start: Int = 0, acc: List[Int] = Nil): List[Int] = where.indexOf(string, start) match {
      case -1 => acc.reverse
      case index => inclusionsInto(where, index + 1, index :: acc)
    }


    @tailrec final def firstMatch(where: String, currentPosition: Int = 0, stopBeforeEnd: Int = 0)(compareFun: (String, String, Int) => Boolean): Int =
      if (currentPosition + string.length + stopBeforeEnd > where.length)
        -1
      else if (compareFun(string, where, currentPosition)) currentPosition else firstMatch(where, currentPosition + 1, stopBeforeEnd)(compareFun)


    /**
      * Find a substring in $where that matches specific conditions
      *
      * @param where string inside which substrings are searched
      * @param current current index
      * @param startAfter char number
      * @param stopBeforeEnd chars before end
      * @param acc accumulator
      * @param compareFun a function (what, where) => Boolean to assess the substring match
      * @return
      */
    @tailrec final def matchesIn(where: String,
                                 current: Int = 0,
                                 startAfter: Int = 0,
                                 stopBeforeEnd: Int = 0,
                                 acc: List[Int] = Nil)
                                (compareFun: (String, String, Int) => Boolean): List[Int] = {
      if (current < startAfter)
        matchesIn(where, startAfter, startAfter, stopBeforeEnd, acc)(compareFun)
      else if (current + string.length + stopBeforeEnd > where.length) acc.reverse
      else
        firstMatch(where, current, stopBeforeEnd)(compareFun) match {
          case -1 => acc.reverse
          case index =>
            matchesIn(where, index + 1, startAfter, stopBeforeEnd, index :: acc)(compareFun)
        }
    }

    @tailrec final def compareWithMismatches(what: String,
                                             where: String,
                                             maxMismatches: Int,
                                             start: Int,
                                             current: Int = 0,
                                             acc: Int = 0
                                            ): Boolean =
      if(acc > maxMismatches)
        false
      else
      if(current >= what.length)
        true
      else
      if (current + start >= where.length)
        false
      else
      if (basesEqual(what(current).toUpper, where(start + current).toUpper))
        compareWithMismatches(what, where,  maxMismatches, start, current + 1, acc)
      else
        compareWithMismatches(what, where, maxMismatches, start, current + 1, acc + 1)

    def partialMatchesIn(where: String, maxMismatches: Int, current: Int = 0,
                         startAfter: Int = 0,
                         stopBeforeEnd: Int = 0): List[Int] =
      this.matchesIn(where, current, startAfter, stopBeforeEnd) {
        case (what, wh, start) => compareWithMismatches(what, wh, maxMismatches, start)
      }
  }
}