package snakemazia.gamelogic.terrain

import java.awt.Point
import java.io.File
import Part._
import snakemazia.util.Utils.Arrays._
import snakemazia.util.Utils.Random._
import snakemazia.gamelogic.Direction
import annotation.tailrec
import snakemazia.util.Utils.Random

/**
 * User: ceyhun
 * Date: 2/12/13
 * Time: 5:57 PM
 * 
 */
final class Terrain(landscape: Array[Array[Part]]) {
  require(landscape.isRectangular, "Landscape has to be rectangular!")

  val nonSpaces: Array[Point] =
    for {
      (arr, x) <- landscape.zipWithIndex
      (part, y) <- arr.zipWithIndex
      if part == Block
    } yield new Point(x, y)


  private val neighborhoods: Map[Point, Map[Direction, Point]] =
    (
      for {
        (arr, x) <- landscape.zipWithIndex
        (part, y) <- arr.zipWithIndex
        point = new Point(x, y)
        if isOver(point)
        neighbors = (for {
          dir <- Direction.directions
          neighbor = dir.alongThis(point)
          if isOver(neighbor)
        } yield dir -> neighbor).toMap
        if !neighbors.isEmpty
      } yield point -> neighbors
    ).toMap

  def spaceWidth = landscape.length
  def spaceHeight = landscape(0).length

  def partAt(cor: Point) = if (isOut(cor)) Space else landscape(cor.x)(cor.y)
  def isOver(cor: Point) = partAt(cor) == Block
  private def isOut(cor: Point) = cor.x < 0 || cor.x >= spaceWidth || cor.y < 0 || cor.y >= spaceHeight

  def randomNonSpace: Point = nonSpaces.toSeq.randomElement

  def randTrail(trailLength: Int): Vector[Point] = {
    @tailrec
    def walk(currentWalk: Vector[Point], n: Int): Option[Vector[Point]] = n match {
      case 0 => Some(currentWalk)
      case _ => {
        (for {
          neighborhood <- neighborhoods.get(currentWalk.last)
          l <- neighborhood.values.filterNot { currentWalk contains _ }.randomOption
        } yield l
        ) match {
          case Some(neighbor) => walk(currentWalk :+ neighbor, n - 1)
          case _           => None
        }
      }
    }
    def start = walk(Vector(randomNonSpace), trailLength - 1)
    //try until you find a walk that is long enough, indefinitely
    (Stream continually { start } collectFirst { case Some(points) => points }).get
  }

}

object Terrain {

  def random(w: Int, h: Int, blockProb: Double = 0.7): Terrain = {
    import Random._
    Terrain(Array.fill(w, h) {
      Part.chars(if (luckierThan(blockProb)) 1 else 0)
    })
  }


  /**
   * # # # #
   * # _ # #
   * # _ _ _
   * # # # #
   *
   * @return
   */
  def exampleTerrain: Terrain = Terrain(
                                 "####",
                                 "#__#",
                                 "##_#",
                                 "##_#")

  def apply(array: String*): Terrain = apply(array.toArray)

  def apply(lines: Array[String]): Terrain = apply(lines.map(_.toArray))

  def apply(array: Array[Array[Char]]): Terrain =
    new Terrain(array.map{_.map{ Part(_) }})

  /**
   * Build full terrain.
   * @param w Width of terrain.
   * @param h Height of terrain.
   * @return A terrain with specified width and height and contains no space.
   */
  def apply(w: Int, h: Int): Terrain = new Terrain(Array.fill(w, h) { Block })

  object TerrainIO {

    def loadTerrainData(filename: String): Array[String] = loadTerrainData(new File(filename))

    def loadTerrainData(file: File): Array[String] = io.Source.fromFile(file).getLines().toArray

  }

}

