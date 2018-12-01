package assembly

import assembly.synthesis._

import scala.util._
import org.scalatest._
import cats.implicits._

class SequenceGeneratorSpec extends WordSpec with Matchers {

  "Sequence generator" should {
    "generate sequence with proper GC" in {
      lazy val generator = new SequenceGenerator

      val sequence = "GNNCTNGCCTTCGTTGGAAACGGAYATNTGGNSNKNTNTNNGGNCTTCNYATAANNNCTAGANGNGTGNNCANAGGTNAACMNTYCNTTNSANGGNNCGAAACATTCTNAGNAGANNTTKGAAACTTTCTNTGTGAGCGCTCACAAANNGGTGTGAGCGCTCACATGRANA"
      val avoid = Set("AgeI", "SpeI", "NotI", "PstI", "EcoRI", "NotI", "XbaI", "NgoMIV")

      lazy val default = GenerationParameters(StringTemplate(sequence), 20, avoid, ContentGC.default)

      lazy val parameters = default

        for(_ <- 1 to 10){
          val result = generator.generateRepeat(parameters).get
          parameters.check(result) shouldEqual true
          val gc = parameters.contentGC.countGC(result)
          val percent = gc / result.length.toDouble
          percent shouldEqual parameters.contentGC.ratioGC(result)
          assert(percent >= parameters.contentGC.minTotal && percent <= parameters.contentGC.maxTotal)
        }
    }
  }

}
