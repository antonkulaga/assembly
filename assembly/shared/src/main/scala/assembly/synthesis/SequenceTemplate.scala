package assembly.synthesis

import assembly.extensions._

import io.circe.generic.JsonCodec
import scala.collection.SortedSet
import io.circe.syntax._
import cats.implicits._
import io.circe.syntax._

@JsonCodec case class StringTemplate(string: String) extends SequenceTemplate with SequenceTemplatePositional {

  override def withSequenceReplacement(sequence: String, position: Int): SequenceTemplate = {
    val newStr = string.slice(0, position) + sequence + string.slice(position + sequence.length, string.length)
    copy(string = newStr)
  }

  def randomize: String = string.randomize

  override def positionalRandomize(from: Int, len: Int, otherSpans: (Int, Int)*): String = {
    val start = string.substring(0, from)
    val end1 = from + len
    val firstRandom: String = string.substring(from, end1).randomize
    if(otherSpans.isEmpty) start + firstRandom + string.substring(end1)
    else if(otherSpans.length == 1 ) {
      val from2 = otherSpans.head._1
      val len2 = otherSpans.head._2
      val end2 = from2 + len2
      start + firstRandom + string.substring(end1, from2) + string.substring(from2, end2).randomize + string.substring(end2)
    }
    else
    {
      throw new NotImplementedError("randomization of more than two spans has not implemented yet!")
    }
  }

}

object SequenceTemplatePositional
trait SequenceTemplatePositional extends SequenceTemplate {

  def positionalRandomize(from: Int, len: Int, spans: (Int, Int)*): String
}

object SequenceTemplate
trait SequenceTemplate {
  def randomize: String
  //def slice(from: Int, to: Int): SequenceTemplate
  def withSequenceReplacement(sequence: String, position: Int): SequenceTemplate
}