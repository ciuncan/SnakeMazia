package snakemazia.gamelogic.entities

import java.awt.Point
import snakemazia.util.Utils.Random

/**
 * User: ceyhun
 * Date: 2/13/13
 * Time: 4:59 AM
 * 
 */
sealed abstract class Berry(val point: Point, val score: Int, val ttl: Float) extends Locatable {
  var timeAlive: Float = _

  private val randSeed: Float = Random.rand()

  def animationValue: Float = randSeed + timeAlive

  def shouldRemove = timeAlive > ttl

  def update(tpf: Float) {
    timeAlive += tpf
    //println(s"$this -> $timeAlive, soshite remove? $shouldRemove")
  }

  //TODO these might be unnecessary
  override def equals(a: Any) = super.equals(a)
  override def hashCode = super.hashCode
  override def toString = s"${getClass.getSimpleName}{point=$point,score=$score,ttl=$ttl,timeAlive=$timeAlive}"

}

class BlueBerry(point: Point, score: Int = 5, ttl: Float = 1500f) extends Berry(point, score, ttl)
class HeartBerry(point: Point, score: Int = 50, ttl: Float = 500f) extends Berry(point, score, ttl)

object Berry {

  def apply(point: Point): Berry =
    if (Random luckierThan 0.9) new HeartBerry(point)
    else new BlueBerry(point)

}

