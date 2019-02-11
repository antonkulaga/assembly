package assembly.extensions

import scala.annotation.tailrec
import scala.collection.immutable.Map

trait CharFunctions extends ExtensionsBase {

  def getRandomChar(char: Char)(mp: Map[Char, Array[Char]]): Char = mp.get(char) match {
    case Some(a) => arr(a)
    case None => char
  }

  def randomizeChar(char: Char): Char = getRandomChar(char)(nucs)

  def complementChar(char: Char): Char = char.toUpper match {
    case 'A' => 'T'
    case 'T' => 'A'
    case 'G' => 'C'
    case 'C' => 'G'
    case '-' => '-' //gap
    case other => other //not sure if I should throw here
  }

}

