package snakemazia.gamelogic.entities

import snakemazia.gamelogic.{North, Direction}
import java.awt.Point

/**
 * User: ceyhun
 * Date: 2/12/13
 * Time: 6:26 PM
 *
 */
class Snake(private val locations: Seq[Point]) {

  require(!locations.isEmpty, "There should be at least one location for head!")

  val sHead = SnakeHead

  var parts: List[SnakePart] = sHead :: {
    val rest =  for ((headSide, tailSide) <- locations zip locations.tail)
                  yield new SnakeRest(tailSide, Direction(from = tailSide, to = headSide))
    sHead.point = locations.head
    rest match {
      case Seq(s, _*) => sHead.dir = s.dir
      case _      => sHead.dir = North
    }
    rest.toList
  }

  def sTail: List[SnakePart] = parts.tail

  private var nextDirection = sHead.dir

  private[gamelogic] def addPartToEnd(): SnakeRest = {
    val (prevLast, last) = (parts zip parts.tail).last
    val direct = Direction(from = last, to = prevLast)
    val part = new SnakeRest(direct alongThis last, direct)
    parts = parts ++ List(part)
    part
  }

  private[gamelogic] def move() {
    followHead()
    sHead.moveAlong()
    sHead.dir = nextDirection
  }

  private[gamelogic] def readyToTurnLeft() {
    nextDirection = nextDirection.prev
  }

  private[gamelogic] def readyToTurnRight() {
    nextDirection = nextDirection.next
  }

  def nextDirections = nextDirection :: ( parts.init map { _.dir } )

  /**
   * Tails follow heads, all but head moves forward
   */
  private def followHead() {
    for ((headSide, tailSide) <- (parts zip parts.tail).reverse) {
      tailSide moveAlong Direction(from = tailSide, to = headSide)
      tailSide.dir = headSide.dir
    }
  }

}

