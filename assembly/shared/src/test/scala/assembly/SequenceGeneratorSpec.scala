package assembly

import assembly.cloning.RestrictionEnzymes
import assembly.synthesis._

import scala.util._
import org.scalatest._
import cats.implicits._

class SequenceGeneratorSpec extends WordSpec with org.scalatest.matchers.should.Matchers {

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

    "change only part of the sequence" in {
      lazy val generator = new SequenceGenerator

      val sequence = "N" * 20
      val template = StringTemplate(sequence)
      lazy val default = GenerationParametersImpl(template, 20, ContentGC.default, RestrictionEnzymes.GOLDEN_GATE)
      lazy val parameters = default
      val base = generator.tryRandomize(parameters, 10000).get
      parameters.check(base) shouldEqual true
      //val gc = parameters.contentGC.countGC(base)
      val fixed = base.slice(3, base.length -3)
      val newString: String = template.string.take(3) + fixed + template.string.takeRight(3)
      val newTemplate = template.copy(string = newString)
      val newParameters = default.copy(template = newTemplate)
      for(_ <- 1 to 10){
        val result = generator.tryRandomize(newParameters, 10000).get
        assert(result.contains(fixed) && result != base && result != newString, "randomized result should not be equal base but should contain fixed part")
        newParameters.check(result) shouldEqual true
        val gc = newParameters.contentGC.countGC(result)
        val percent = gc / result.length.toDouble
        percent shouldEqual newParameters.contentGC.ratioGC(result)
        assert(percent >= newParameters.contentGC.minTotal && percent <= parameters.contentGC.maxTotal)
      }
    }
  }

}
