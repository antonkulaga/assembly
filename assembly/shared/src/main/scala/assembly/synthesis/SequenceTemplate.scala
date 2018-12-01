package assembly.synthesis

import assembly.extensions._

import io.circe.generic.JsonCodec
import scala.collection.SortedSet
import io.circe.syntax._
import cats.implicits._

@JsonCodec case class StringTemplate(str: String) extends SequenceTemplate {
  def randomize: String = str.randomize
}

trait SequenceTemplate {
  def randomize: String
}