package snakemazia.gamelogic

import entities.Locatable
import java.awt.Point

/**
 * User: ceyhun
 * Date: 2/13/13
 * Time: 5:14 AM
 * 
 */
sealed abstract class Direction(ind: Int, val dx: Int, val dy: Int) {

  def next: Direction = Direction.directions((ind + 1) % 4)
  def prev: Direction = Direction.directions((4 + ind - 1) % 4)

  def alongThis(p: Point, amount: Int = 1) = new Point(p.x + amount * dx, p.y + amount * dy)

  def unary_- = Direction(-dx, -dy)

}

case object North extends Direction(0,  0, -1)
case object East  extends Direction(1,  1,  0)
case object South extends Direction(2,  0,  1)
case object West  extends Direction(3, -1,  0)

object Direction {

  assert(directions forall { dir => isKnownDirection(dir.dx, dir.dy) },
    "Unknown directions among the known directions! :D")

  def directions: Array[Direction] = Array(North, East, South, West)

  def apply(dx: Int, dy: Int): Direction =
    if       (dx > 0 && dy == 0) East
    else if (dx < 0 && dy == 0) West
    else if (dx == 0 && dy > 0) South
    else if (dx == 0 && dy < 0) North
    else throw new UnknownDirectionException(dx, dy)

  def isKnownDirection(dx: Int, dy: Int): Boolean =
    (dx > 0 && dy == 0) || (dx < 0 && dy == 0) || (dx == 0 && dy > 0) || (dx == 0 && dy < 0)

  def isKnownDirection(from: Point, to: Point): Boolean = isKnownDirection(to.x - from.x, to.y - from.y)

  def apply(from: Point, to: Point): Direction = Direction(to.x - from.x, to.y - from.y)

  //check this
  def unapply(dir: Direction): Option[Direction] = {
    var ret: Option[Direction] = None
    try ret = Some(apply(dir.dx, dir.dy))
    ret
  }

  class UnknownDirectionException(dx: Int, dy: Int)
      extends RuntimeException(s"Unknown direction dx:$dx, dy:$dy!")

}


