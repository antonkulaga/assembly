package assembly.synthesis

import assembly.cloning.{RestrictionEnzyme, _}
import assembly.extensions._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Sequence generator for golden gate
  * @param enzyme
  */
case class  SequenceGeneratorGold(goldenGate: GoldenGate, minStickyDifference: Int = 2, minGCbases: Int = 1) extends SequenceGenerator{

  final def randomizeMany(parameters: GenerationParameters, number: Int, maxTries: Int, acc: List[String] = Nil): List[String] = if(number==0) acc.reverse else {
    val result = randomize(parameters, maxTries)
    if(goldenGate.checkEnds(result, acc, minStickyDifference, minGCbases)) randomizeMany(parameters, number - 1, maxTries, result::acc) else randomizeMany(parameters, number, maxTries - 1, acc)
  }

  def generateMany(parameters: GenerationParameters, number: Int, maxTries: Int, stickyLeft: String, stickyRight: String): List[String] = {
    val sequences = randomizeMany(parameters, number, maxTries)
    goldenGate.synthesize(sequences, stickyLeft, stickyRight)
  }

}


object SequenceGenerator extends SequenceGenerator
class SequenceGenerator{

  @tailrec final def randomize(parameters: GenerationParameters, max: Int, count: Int = 0): String = if(max>0){
    val result = parameters.template.randomize
    if(parameters.check(result)) result else randomize(parameters, max -1, count + 1)
  } else {
    throw new Exception(s"We tried to generate a proper sequence more than ${count} times and failed!")
  }


  def tryRandomize(parameters: GenerationParameters, maxTries: Int ) = Try {
    //assert( parameters.check(parameters.template), "Template should not contain restriction sites that must be avoided")
    //assert( parameters.checkGC(parameters.template), "Template should fit into GC requirements")
    randomize(parameters, maxTries)
  }

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
             gcContent: ContentGC,
             restrictions: RestrictionEnzymes): GenerationParameters = new GenerationParameters {
    override def template: SequenceTemplate = sequenceTemplate

    override def maxRepeatSize: Int = maximumRepeatSize

    override def contentGC: ContentGC = gcContent

    override def enzymes: RestrictionEnzymes = restrictions
  }
}

trait GenerationParameters{

  def template: SequenceTemplate
  def maxRepeatSize: Int
  def contentGC: ContentGC
  def enzymes: RestrictionEnzymes


  def check(sequence: String): Boolean = checkRepeats(sequence) && checkGC(sequence) && checkEnzymes(sequence)

  def checkEnzymes(sequence: String): Boolean = !enzymes.canCut(sequence, true)

  def checkGC(sequence: String): Boolean = contentGC.checkGC(sequence)

  def checkRepeats(sequence: String): Boolean = repeats(sequence, maxRepeatSize).isEmpty

  def repeats(str: String, length: Int): Map[String, Int] = {
    val slide = str.sliding(length)
    slide.foldLeft(Map.empty[String, Int]){
      case (acc, s) => if(acc.contains(s)) acc.updated(s, acc(s) + 1) else acc.updated(s, 1)
    }.filter(_._2 > 1)
  }


  def findLongestRepeats(sequence: String, length: Int = maxRepeatSize): Map[String, Int] = if(length == 1) Map.empty else
  {
    val mp = repeats(sequence, length)
    if(mp.isEmpty) findLongestRepeats(sequence, length -1) else mp
  }
}
