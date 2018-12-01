package assembly.synthesis

import assembly.cloning._
import io.circe.generic.JsonCodec

import scala.util.Try


object SequenceGenerator extends SequenceGenerator
class SequenceGenerator{

  def randomize(parameters: GenerationParameters, max: Int = 1000): String = if(max>0){
    val result = parameters.template.randomize
    if(parameters.check(result)) result else randomize(parameters, max -1)
  } else {
    throw new Exception(s"We tried to generate a proper sequence more than ${max} times and failed!")
  }


  def generateRepeat(parameters: GenerationParameters, maxTries: Int = 1000 ) = Try {
    //assert( parameters.check(parameters.template), "Template should not contain restriction sites that must be avoided")
    //assert( parameters.checkGC(parameters.template), "Template should fit into GC requirements")
    randomize(parameters, maxTries)
  }

  def generateHOR(prefix: String, suffix: String, parameters: GenerationParameters) = ???
}


object GenerationParameters {
  /*
  implicit def monoid: cats.Monoid[GenerationParameters] = new cats.Monoid[GenerationParameters] {
    override def empty: GenerationParameters = GenerationParameters.default

    override def combine(x: GenerationParameters, y: GenerationParameters): GenerationParameters = y //ugly TODO: rewrite
  }
  */

  def apply( sequenceTemplate: SequenceTemplate,
             maximumRepeatSize: Int,
             enzymesToAvoid: Set[String],
             gcContent: ContentGC,
             restrictions: RestrictionEnzymes = RestrictionEnzymes.default): GenerationParameters = new GenerationParameters {
    override def template: SequenceTemplate = sequenceTemplate

    override def maxRepeatSize: Int = maximumRepeatSize

    override def avoidEnzymes: Set[String] = enzymesToAvoid

    override def contentGC: ContentGC = gcContent

    override def enzymesLibrary: RestrictionEnzymes = restrictions
  }
}

trait GenerationParameters{

  def template: SequenceTemplate
  def maxRepeatSize: Int
  def avoidEnzymes: Set[String]
  def contentGC: ContentGC
  def enzymesLibrary: RestrictionEnzymes


  def check(sequence: String): Boolean = checkEnzymes(sequence) && checkRepeats(sequence) && checkGC(sequence)

  def checkEnzymes(sequence: String): Boolean = enzymesLibrary.notIncludesEnzymes(sequence, avoidEnzymes)

  def checkGC(sequence: String): Boolean = contentGC.checkGC(sequence)

  def checkRepeats(sequence: String): Boolean = repeatingSub(sequence, maxRepeatSize).isEmpty

  def repeatingSub(str: String, length: Int): Map[String, Int] = {
    val slide = str.sliding(length)
    slide.foldLeft(Map.empty[String, Int]){
      case (acc, s) => if(acc.contains(s)) acc.updated(s, acc(s) + 1) else acc.updated(s, 1)
    }.filter(_._2 > 1)
  }

  def findLargestRepeats(sequence: String, length: Int = maxRepeatSize): Map[String, Int] = if(length == 1) Map.empty else
  {
    val mp = repeatingSub(sequence, length)
    if(mp.isEmpty) findLargestRepeats(sequence, length -1) else mp
  }
}
