package assembly

import assembly.cloning.{GoldenGate, RestrictionEnzymes}
import assembly.synthesis._
import assembly.extensions._

import scala.util._
import org.scalatest._
import cats.implicits._

import scala.util._
import org.scalatest._
import cats.implicits._

class GoldenGateSpec extends WordSpec with Matchers{

  "Golden gate assembly" should {
    "suggest stitching" in {
      val whole    = "ATTTGGAGAGCTCTGAAAATTTCGTAGGAAATGTGAGCGCTCACAAATAAAATCAAGACAGAAGCATTCTCAGAAACCTCTTTGTG"
      val site = "GGTCTC"
      val revCom = "GAGACC"

      val one = "ATTTGGAGAGCTCTGAAAATTT"
      val two = "CGTAGGAAATGTGAGCGCTCACAAATAAAAT"
      val three = "CAAGACAGAAGCATTCTCAGAAACCTCTTTGTG"

      val gold = GoldenGate(RestrictionEnzymes.BsaI)
      val flankLeft = "CCTG"
      val flankRight = "GGAA"
      val oneS::twoS::threeS::Nil = gold.synthesize(one::two::three::Nil, flankLeft, flankRight)

      val oneR: String = "GGTCTCCCCTGATTTGGAGAGCTCTGAAAATTTCGTACGAGACC"
      oneS shouldEqual oneR
      val twoR: String = "GGTCTCCCGTAGGAAATGTGAGCGCTCACAAATAAAATCAAGCGAGACC"
      twoS shouldEqual  twoR
      val threeR: String = "GGTCTCCCAAGACAGAAGCATTCTCAGAAACCTCTTTGTGGGAACGAGACC"
      threeS shouldEqual threeR

      println(oneR)
      println(twoR)
      println(threeR)

    }

    /*
    "suggest golden-gate flanking to any type of sequences that do not have GoldenGate sites" in {
      lazy val generator = new SequenceGenerator

      val sequence = "NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN"
      //val avoid = Set("AgeI", "SpeI", "NotI", "PstI", "EcoRI", "NotI", "XbaI", "NgoMIV")
      //val bioBrick = RestrictionEnzymes(RestrictionEnzymes.commonEnzymesSet.filter{ case (e, _)=> avoid.contains(e)})

      lazy val parameters: GenerationParameters = GenerationParameters(StringTemplate(sequence), 20, ContentGC.default, RestrictionEnzymes.GOLDEN_GATE)

      for(_ <- 1 to 10){
        val one = generator.randomize(parameters, 10000)
        val two = generator.randomize(parameters, 10000)
        val three = generator.randomize(parameters, 10000)
        val syn = GoldenGate(RestrictionEnzymes.BsaI)


      }
    }
    */
  }
  /*
   lazy val default = GenerationParameters(StringTemplate(sequence), 20, ContentGC.default, RestrictionEnzymes.GOLDEN_GATE)

      lazy val parameters = default

      for(_ <- 1 to 10){
        val result = generator.generateRepeat(parameters, 10000).get
        parameters.check(result) shouldEqual true
        val gc = parameters.contentGC.countGC(result)
        val percent = gc / result.length.toDouble
        percent shouldEqual parameters.contentGC.ratioGC(result)
        assert(percent >= parameters.contentGC.minTotal && percent <= parameters.contentGC.maxTotal)
      }
   */
}
