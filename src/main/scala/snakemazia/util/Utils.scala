package snakemazia.util

import util.Random
import java.awt.Point

/**
 * User: ceyhun
 * Date: 2/13/13
 * Time: 3:45 PM
 * 
 */
object Utils {

  object Arrays {

    implicit class RicherArray[T](arr: Array[Array[T]]) {
      def isRectangular: Boolean = arr.foldLeft(true) { (b, ar) => b && (arr(0).length == ar.length) }
    }

  }

  object Random {

    private val random = new Random()

    def rand(): Float = random.nextDouble().toFloat

    def luckierThan(d: Double): Boolean = rand > d

    def randRange(low: Int, high: Int): Int = random.nextInt(high - low) + low
    def randUpTo(high: Int): Int = randRange(0, high)

    def randPoint(lowX: Int, highX: Int)(lowY: Int, highY: Int): Point =
      new Point(randRange(lowX, highX), randRange(lowY, highY))
    def randPoint(highX: Int)(highY: Int): Point = randPoint(0, highX)(0, highY)

    implicit class RandomizableArray[T](seq: Iterable[T]) {
      def randomElement: T = seq.toIndexedSeq.apply(randUpTo(seq.size))
      def randomOrder: Seq[T] = random.shuffle(seq.toSeq)
      def randomOption: Option[T] = if (seq.isEmpty) None else Some(randomElement)
    }

  }

}

