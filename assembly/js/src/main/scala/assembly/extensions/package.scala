package assembly

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.util.Random

package object extensions extends StringFunctions(new java.util.Random) {
  self =>

  implicit class Nucleotide(val char: Char) extends AnyVal {

    def getRandom(mp: Map[Char, Array[Char]]): Char = getRandomChar(char)(mp)

    def complement: Char = complementChar(char)

    def randomize: Char = getRandom(nucs)

  }

  /*  *
    * Implicit class that extends string with sequences with extra methods
    * @param str
    */
  implicit class StringSeq(val string: String) extends AnyVal {

    def complement: String = complementString(string)

    def reverseComplement: String = reverseComplementString(string)

    def randomize: String = self.randomizeString(string)

    /**
      * Finds all inclusions of current string into where string
      * @param where
      * @param start at which index to start
      * @param acc accumulator required for recursive
      * @return List of indexes found
      */
    def inclusionsInto(where: String, start: Int = 0, acc: List[Int] = Nil): List[Int] = self.inclusionsInto(string, where, start, acc)

    def firstMatch(where: String, currentPosition: Int = 0, stopBeforeEnd: Int = 0)(compareFun: (String, String, Int) => Boolean): Int =
      self.firstMatch(string, where, currentPosition, stopBeforeEnd)(compareFun)

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
    def matchesIn(where: String,
                                 current: Int = 0,
                                 startAfter: Int = 0,
                                 stopBeforeEnd: Int = 0,
                                 acc: List[Int] = Nil)
                                (compareFun: (String, String, Int) => Boolean): List[Int] =self.matchesIn(string, where, current, startAfter, stopBeforeEnd, acc)(compareFun)

    def partialMatchesIn(where: String, maxMismatches: Int, current: Int = 0,
                         startAfter: Int = 0,
                         stopBeforeEnd: Int = 0): List[Int] =
      this.matchesIn(where, current, startAfter, stopBeforeEnd) {
        case (what, wh, start) => self.compareWithMismatches(what, wh, maxMismatches, start)
      }
  }
}
