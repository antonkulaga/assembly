package assembly
import assembly.cloning.RestrictionEnzymes
import assembly.synthesis._
import assembly.extensions._

import scala.util._
import org.scalatest._
import cats.implicits._

class CloningSpec extends WordSpec with Matchers  {
  "Restriction enzymes" should {

    "find all instances" in {
      lazy val generator = new SequenceGenerator

      val sequence = "CAACACNTNNNGGTCTCNNGNNGGTCTCCAACAC"

      lazy val parameters = GenerationParameters(StringTemplate(sequence), 20, ContentGC.default, RestrictionEnzymes.GOLDEN_GATE)
      for(_ <- 0 until 1000 ){
        val seq = generator.tryRandomize(parameters, 1000).get
        val found = RestrictionEnzymes.GOLDEN_GATE.find(seq, false)
        found shouldEqual
            Map(
              ("BsaI", "GGTCTC") -> List(11, 22),
              ("BsbI", "CAACAC") -> List(0, 28)
            )
      }
      //  "BsbI" -> "CAACAC"
      // "BsaI" -> "GGTCTC",
    }

    "process forward and reverse cuts properly" in {
      val site = "GGTCTC"
      val revCom = "GAGACC"
      revCom shouldEqual site.reverseComplement
      val seq = "CAACACNTNNNGGTCTCNNGNNGGTCTCCAACAC" + "GAGACC"
      val bsaI = RestrictionEnzymes.BsaI
      bsaI.positionsForward(seq) shouldEqual List(11, 22)
      bsaI.positions(seq)._2 shouldEqual List(34)
      val (fw, rw) = bsaI.cuts(seq)
      fw shouldEqual List( (18, 22), (29, 33))
      rw shouldEqual List( (29,  33))

    }
  }

}
