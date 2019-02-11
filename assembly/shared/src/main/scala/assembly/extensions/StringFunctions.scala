package assembly.extensions

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.util.Random

class StringFunctions(random: java.util.Random) extends ExtensionsBase with CharFunctions {

  protected def arr[T](items: Array[T]):T = {
    items(random.nextInt(items.length))
  }

  def complementString(string: String): String =  string.toUpperCase.map {
    case 'A' => 'T'
    case 'T' => 'A'
    case 'G' => 'C'
    case 'C' => 'G'
    case other => other //not sure if I should throw here
  }

  def reverseComplementString(string: String): String = complementString(string.reverse)

  def compareSeq(what: String, where: String, start: Int): Boolean = what.indices
    .forall{ i=> basesEqual(what(i).toUpper, where(start + i).toUpper) }

  /**
    * Finds all inclusions of current string into where string
    * @param where
    * @param start at which index to start
    * @param acc accumulator required for recursive
    * @return List of indexes found
    */
  @tailrec final def inclusionsInto(string: String, where: String, start: Int = 0, acc: List[Int] = Nil): List[Int] = where.indexOf(string, start) match {
    case -1 => acc.reverse
    case index => inclusionsInto(string, where, index + 1, index :: acc)
  }

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
  @tailrec final def matchesIn(string: String, where: String,
                               current: Int = 0,
                               startAfter: Int = 0,
                               stopBeforeEnd: Int = 0,
                               acc: List[Int] = Nil)
                              (compareFun: (String, String, Int) => Boolean): List[Int] = {
    if (current < startAfter)
      matchesIn(string,where, startAfter, startAfter, stopBeforeEnd, acc)(compareFun)
    else if (current + string.length + stopBeforeEnd > where.length) acc.reverse
    else
      firstMatch(string, where, current, stopBeforeEnd)(compareFun) match {
        case -1 => acc.reverse
        case index =>
          matchesIn(string, where, index + 1, startAfter, stopBeforeEnd, index :: acc)(compareFun)
      }
  }



  @tailrec final def firstMatch(string: String, where: String, currentPosition: Int = 0, stopBeforeEnd: Int = 0)(compareFun: (String, String, Int) => Boolean): Int =
    if (currentPosition + string.length + stopBeforeEnd > where.length)
      -1
    else if (compareFun(string, where, currentPosition)) currentPosition else firstMatch(string, where, currentPosition + 1, stopBeforeEnd)(compareFun)

  def randomizeString(string: String): String =   {
    string.toUpperCase.map(s=>randomizeChar(s))
  }

  def seqsInclusionsInto(what: String, where: String): List[Int] = {
    matchesIn(what, where, 0)(compareSeq)
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

  def partialMatchesIn(string: String, where: String, maxMismatches: Int, current: Int = 0,
                       startAfter: Int = 0,
                       stopBeforeEnd: Int = 0): List[Int] =
    this.matchesIn(string, where, current, startAfter, stopBeforeEnd) {
      case (what, wh, start) => compareWithMismatches(what, wh, maxMismatches, start)
    }

}