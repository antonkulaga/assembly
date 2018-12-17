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


    "suggest golden-gate flanking to any type of sequences that do not have GoldenGate sites" in {
      lazy val generator = new SequenceGeneratorGold(GoldenGate(RestrictionEnzymes.BsaI))
      val site = "GGTCTC"
      val revCom = "GAGACC"

      val sequence = "NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN"
      //val avoid = Set("AgeI", "SpeI", "NotI", "PstI", "EcoRI", "NotI", "XbaI", "NgoMIV")
      //val bioBrick = RestrictionEnzymes(RestrictionEnzymes.commonEnzymesSet.filter{ case (e, _)=> avoid.contains(e)})

      lazy val parameters: GenerationParameters = GenerationParameters(StringTemplate(sequence), 20, ContentGC.default, RestrictionEnzymes.GOLDEN_GATE)

      for(_ <- 1 to 10){
       // generator.randomizeMany(parameters, 3, )
        val generated  = generator.randomizeMany(parameters, 20, 10000)
        val gold = GoldenGate(RestrictionEnzymes.BsaI)
        val flankLeft = "CCTG"
        val flankRight = "GGAA"
        val gen_syn = gold.synthesize(generated, flankLeft, flankRight)
        val firstS = gen_syn.head
        val lastS = gen_syn.last
        firstS.startsWith(site +"C" + flankLeft) shouldEqual true
        lastS.endsWith(flankRight +"C" + revCom) shouldEqual true
        val fourS = gen_syn(4)
        val fiveS = gen_syn(5)
        gen_syn.forall(_.startsWith(site)) shouldEqual  true
        gen_syn.forall(_.endsWith("C" + revCom)) shouldEqual  true
        fourS.substring(fourS.length - gold.enzyme.forwardCut - 4, fourS.length - gold.enzyme.forwardCut) shouldEqual
          fiveS.substring(gold.enzyme.forwardCut, gold.enzyme.forwardCut + Math.abs(gold.enzyme.stickyLength))

      }
    }
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
