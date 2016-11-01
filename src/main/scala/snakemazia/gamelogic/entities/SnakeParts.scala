package snakemazia.gamelogic.entities

import snakemazia.gamelogic.{North, Direction}
import java.awt.Point

/**
 * User: ceyhun
 * Date: 2/12/13
 * Time: 6:04 PM
 *
 */

abstract sealed class SnakePart(var point: Point, var dir: Direction) extends Locatable {

  def moveAlong(direct: Direction = dir) {
    point = direct alongThis point
    dir = direct
  }

  override def equals(a: Any) = super.equals(a)
  override def hashCode = super.hashCode
  override def toString = s"${getClass.getSimpleName}{point=$point,dir=$dir}"

}

object SnakeHead extends SnakePart(new Point(), North)

class SnakeRest(point: Point, dir: Direction) extends SnakePart(point, dir)






