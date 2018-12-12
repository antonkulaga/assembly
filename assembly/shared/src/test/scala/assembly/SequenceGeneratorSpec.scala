package assembly

import assembly.cloning.RestrictionEnzymes
import assembly.synthesis._

import scala.util._
import org.scalatest._
import cats.implicits._

class SequenceGeneratorSpec extends WordSpec with Matchers {

  "Sequence generator" should {
    "generate sequence with proper GC" in {
      lazy val generator = new SequenceGenerator

      val sequence = "GNNCTNGCCTTCGTTGGAAACGGAYATNTGGNSNKNTNTNNGGNCTTCNYATAANNNCTAGANGNGTGNNCANAGGTNAACMNTYCNTTNSANGGNNCGAAACATTCTNAGNAGANNTTKGAAACTTTCTNTGTGAGCGCTCACAAANNGGTGTGAGCGCTCACATGRANA"
      //val avoid = Set("AgeI", "SpeI", "NotI", "PstI", "EcoRI", "NotI", "XbaI", "NgoMIV")
      //val bioBrick = RestrictionEnzymes(RestrictionEnzymes.commonEnzymesSet.filter{ case (e, _)=> avoid.contains(e)})

      lazy val default = GenerationParameters(StringTemplate(sequence), 20, ContentGC.default, RestrictionEnzymes.GOLDEN_GATE)

      lazy val parameters = default

      for(_ <- 1 to 10){
        val result = generator.tryRandomize(parameters, 10000).get
        parameters.check(result) shouldEqual true
        val gc = parameters.contentGC.countGC(result)
        val percent = gc / result.length.toDouble
        percent shouldEqual parameters.contentGC.ratioGC(result)
        assert(percent >= parameters.contentGC.minTotal && percent <= parameters.contentGC.maxTotal)
      }
    }
  }

}
