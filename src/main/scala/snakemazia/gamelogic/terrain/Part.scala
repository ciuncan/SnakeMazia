package snakemazia.gamelogic.terrain

/**
 * User: ceyhun
 * Date: 2/12/13
 * Time: 5:58 PM
 * 
 */
object Part extends Enumeration {

  type Part = Value

  val Space = Value("Space")
  val Block = Value("Block")
  val Unknown = Value("?")

  val charMap = Map('#' -> Block, '_' -> Space)

  lazy val chars = charMap.keys.toArray

  def apply(ch: Char) = charMap getOrElse (ch, Unknown)

}


