package assembly.cloning

/*

/**
  *
  *
    Dam methylase–methylation at the N6 position of the adenine in the sequence GATC (1,2).
    Dcm methyltransferases–methylation at the C5 position of the second cytosine in the sequences CCAGG and CCTGG (1,3).
    EcoKI methylase–methylation of adenine in the sequences AAC(N6)GTGC and GCAC(N6)GTT.
  */

case object Dam extends Methylation("Dam", Set("GATC"), "N", 6)

case object Dcm extends Methylation("Dcm", Set("CCAGG"), "C", 5 )

//case object EcoKI extends Methylation("EcoKI", Set("AAC"), "N", 6)

case class Methylation(name: String, recognizes: Set[String], methylates: String, position: Int) extends SearchDNA {

  def positionsForward(sequence: String): List[Int] = searchesOf(sequence, site)
  def positionsReverseComplement(sequence: String): List[Int] = {
    positionsForward(sequence.reverseComplement).map{ i => sequence.length - site.length - i}
  }
  def positions(sequence: String): (List[Int], List[Int]) = (positionsForward(sequence), positionsReverseComplement(sequence))

}
*/