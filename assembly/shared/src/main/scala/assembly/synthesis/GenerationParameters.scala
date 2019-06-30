package assembly.synthesis

import assembly.cloning.{RestrictionEnzyme, _}
import assembly.extensions._
import scala.collection.compat._
import scala.annotation.tailrec
import scala.util.Try


case class SequenceGeneratorGold(goldenGate: GoldenGate,
                                 minStickyDifference: Int = 2,
                                 minGCbases: Int = 1, maxStickyTries: Int = 256) extends SequenceGenerator{


  protected def prepareStickyTemplate(result: String, parameters: GenerationParameters): GenerationParameters = {
    val slen = goldenGate.enzyme.stickyLength
    val replacement = result.slice(slen, result.length - slen)
    parameters.withReplacement(replacement, slen)
  }


  def generateSticky(result: String, parameters: GenerationParameters, stickyTries: Int,maxTries: Int, acc: List[String] = Nil): Option[String]  =
    if(stickyTries <= 0) None else {
      if(goldenGate.checkEnds(result, acc, minStickyDifference, minGCbases)) Some(result)
      else generateSticky(randomize(parameters, maxTries), parameters, stickyTries -1, maxTries, acc)
    }


  /**
    * Tries to randomize many sequences to fit them into GoldenGate synthesis
    * @param parameters
    * @param number
    * @param maxTries
    * @param maxStickyTries how many tries should be done for randomizing only sticky edges?
    * @param acc
    * @return
    */
  final def randomizeMany(parameters: GenerationParameters,
                          number: Int, maxTries: Int, maxStickyTries: Int = maxStickyTries,
                          acc: List[String] = Nil): List[String] = if(number==0) acc.reverse else {
    val result: String = randomize(parameters, maxTries)
    if(goldenGate.checkEnds(result, acc, minStickyDifference, minGCbases))
      randomizeMany(parameters, number - 1, maxTries, maxStickyTries, result::acc)
    else
    if(maxStickyTries == 0) randomizeMany(parameters, number, maxTries - 1, maxStickyTries, acc)
    else {
      val st = prepareStickyTemplate(result, parameters)
      generateSticky(randomize(st, maxTries), st, maxStickyTries, maxTries, acc) match {
        case Some(v) => randomizeMany(parameters, number - 1, maxTries, maxStickyTries, v::acc)
        case None => randomizeMany(parameters, number, maxTries - 1, maxStickyTries, acc)
      }
    }
  }

  /**
    * Generates many sequences from the same template that will be connected in a GoldenGate way
    * @param parameters
    * @param number
    * @param maxTries
    * @param stickyLeft
    * @param stickyRight
    * @return
    */
  def generateMany(parameters: GenerationParameters, number: Int, maxTries: Int, stickyLeft: String, stickyRight: String, maxStickyTries: Int = 10): List[String] = {
    val sequences = randomizeMany(parameters, number, maxTries, maxStickyTries)
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

/*
case class GenerationParameters(
                                       template: StringTemplate, maxRepeatSize: Int, contentGC: ContentGC, enzymes: RestrictionEnzymes
                                     ) extends GenerationParameters
*/
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
             restrictions: RestrictionEnzymes): GenerationParameters = GenerationParametersImpl(sequenceTemplate, maximumRepeatSize, gcContent, restrictions)
}

trait SequenceGenerationParameters extends GenerationParameters {
  def template: SequenceTemplatePositional//SequenceTemplate

}

case class GenerationParametersImpl(template: SequenceTemplate, maxRepeatSize: Int, contentGC: ContentGC, enzymes: RestrictionEnzymes) extends GenerationParameters
{
  def withReplacement(sequence: String, position: Int): GenerationParametersImpl = copy(template.withSequenceReplacement(sequence, position)) //UGLY part
}

trait GenerationParameters{

  def template: SequenceTemplate
  def maxRepeatSize: Int
  def contentGC: ContentGC
  def enzymes: RestrictionEnzymes

  def withReplacement(sequence: String, position: Int): GenerationParameters  //UGLY part

  def check(sequence: String): Boolean = checkRepeats(sequence) && checkGC(sequence) && checkEnzymes(sequence)

  def checkEnzymes(sequence: String): Boolean = !enzymes.canCut(sequence, true)

  def checkGC(sequence: String): Boolean = contentGC.checkGC(sequence)

  def checkRepeats(sequence: String): Boolean = checkRepeats(sequence, maxRepeatSize)

  /**
    * UGLY MUTABLE!
    * @param str
    * @param len
    * @return
    */
  def  checkRepeats(str: String, len: Int): Boolean = {
    val set = scala.collection.mutable.Set[String]()
    var i = 0
    while(i <= str.length - len){
      val s = str.slice(i, i + len)
      if(set.contains(s)) return false
      set += s
      i += 1
    }
    true
  }

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
