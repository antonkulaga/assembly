package assembly
import assembly.cloning.RestrictionEnzymes
import assembly.synthesis._

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
        val seq = generator.generateRepeat(parameters, 10).get
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
  }

}
