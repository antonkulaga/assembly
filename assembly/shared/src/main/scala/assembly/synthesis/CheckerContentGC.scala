package assembly.synthesis


import io.circe.generic.JsonCodec


trait CheckerGC{

  def countGC(sequence: String): Int = sequence.toUpperCase.count{
    case 'G' | 'C' => true
    case _ => false
  }

  def ratioGC(sequence: String): Double = {
    countGC(sequence) / sequence.length.toDouble
  }

  protected def checkGC(sequence: String, min: Int, max: Int): Boolean = {
    val gc = countGC(sequence)
    (min <= gc) && (gc <= max)
  }

  protected def checkRatioGC(sequence: String, min: Double, max: Double): Boolean = {
    val gc = ratioGC(sequence)
    (min <= gc) && (gc <= max)
  }
}

/**
  * 1. Overall GC content in between 30% to 65%.
  * 2. A window of 100bp with GC content between 25% to 75%.
  * 3. A window of 50bp with GC content between 15% to 80%.
  * 4. No 100% repeat larger than 25bp or with a Tm >= 64°C.
  * 5. Minimal amount of high density small repeats.
  */
@JsonCodec case class WindowGC(size: Int, minGC: Double, maxGC: Double) extends CheckerGC {

  /*
  protected def checkWindow(sequence: String, start: Int, minNumberGC: Int, maxNumberGC: Int, acc: Boolean = true): Boolean = (sequence.length, start) match {

    case (l, s) if s<= l => acc
    case (l, s) =>
      val str = sequence.slice(s, s + size)
      val (gc, total) = (countGC(str), str.length)
      val newStart =
      ???
    //sequence.sliding(size).forall(s=>checkRatioGC(s, minGC, maxGC))

  }
  */

  def checkWindow(sequence: String): Boolean = {
    sequence.sliding(size).forall(s=>checkRatioGC(s, minGC, maxGC))
  }
}

object ContentGC {
  lazy val default = ContentGC(0.3, 0.64,  List(
    WindowGC(100, 0.25, 0.75) ,
    WindowGC(50, 0.15, 0.8)
  ))
}

@JsonCodec case class ContentGC(minTotal: Double,
                                maxTotal: Double,
                                windows: List[WindowGC]
                               ) extends CheckerGC
{
  def checkGC(sequence: String): Boolean = {
    checkRatioGC(sequence, minTotal, maxTotal) && windows.forall(w=>w.checkWindow(sequence))
  }
}