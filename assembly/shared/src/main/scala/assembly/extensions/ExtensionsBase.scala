package assembly.extensions



trait ExtensionsBase {

  protected def arr[T](items: Array[T]): T
  /**
    * Nucleotide code-etters
    */
  val nucs: Map[Char, Array[Char]] = Map(
    'W' -> Array('A', 'T'), //weak
    'S' -> Array('C', 'G'), //strong
    'M' -> Array('A', 'C'), //amino
    'K' -> Array('G', 'T'), //keto
    'R' -> Array('A', 'G'), //purine
    'Y' -> Array('C', 'T'), //pyramidine
    'B' -> Array('C', 'G', 'T'), //not A
    'D' -> Array('A', 'G', 'T'), //not C
    'H' -> Array('A', 'C', 'T'), //not G
    'V' -> Array('A', 'C', 'G'), //not T
    'N' -> Array('A', 'C', 'G', 'T') //any
  )


  def basesEqual(base1: Char, base2: Char): Boolean = (base1, base2) match {
    case (a, b) if a == b => true
    case ('N', _) => true
    case ('V', b) => b != 'T'
    case ('H', b) => b != 'G'
    case ('D', b) => b != 'C'
    case ('B', b) => b != 'A'
    case ('W', b) => b == 'A' || b == 'T' //weak bonds
    case ('S', b) => b == 'G' || b == 'C' //strong bonds
    case ('M', b) => b == 'A' || b == 'C' //amino
    case ('K', b) => b == 'G' || b == 'T' //keto
    case ('Y', b) => b == 'T' || b == 'C' //pyrimidine
    case ('R', b) => b == 'G' || b == 'A' //purine
    case _ => false
  }




}
