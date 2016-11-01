package snakemazia.gamelogic.entities

import java.awt.Point

/**
 * User: ceyhun
 * Date: 2/13/13
 * Time: 2:14 PM
 * 
 */
trait Locatable {

  def x: Int = point.x
  def y: Int = point.y

  def point: Point

  def collides(other: Locatable): Boolean = point == other.point

}

object Locatable {
  implicit def locatableToPoint(l: Locatable): Point = l.point
}
