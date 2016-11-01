package snakemazia.gamelogic

import entities.{SnakePart, Berry, Snake}
import terrain.Terrain
import collection.mutable.ListBuffer
import snakemazia.util.Utils.Random

/**
 * User: ceyhun
 * Date: 2/12/13
 * Time: 5:55 PM
 * 
 */
class GameSkeleton {

  type StateChangedEvent = (GameState) => Unit
  type BerryAddedEvent = (Seq[Berry]) => Unit
  type BerryRemovedEvent = (Seq[Berry]) => Unit
  type SnakePartAddedEvent = (Seq[SnakePart]) => Unit
  type SnakePartMovedEvent = (Snake) => Unit
  type ScoreChangedEvent = (Int) => Unit

  val gameStateHandlers: ListBuffer[StateChangedEvent] = new ListBuffer
  val berryAddedHandlers: ListBuffer[BerryAddedEvent] = new ListBuffer
  val berryRemovedHandlers: ListBuffer[BerryRemovedEvent] = new ListBuffer
  val snakePartAddedHandlers: ListBuffer[SnakePartAddedEvent] = new ListBuffer
  val snakePartMovedHandlers: ListBuffer[SnakePartMovedEvent] = new ListBuffer
  val scoreChangedHandlers: ListBuffer[ScoreChangedEvent] = new ListBuffer

  val terrainWidth = 20
  val terrainHeight = 20
  val initialSnakeLength = 5
  val initialBerryCount = 20
  val diminishInterval = 0.98f
  val terrainSpaceDensity = 0.8f
  val minInterval = 0.2f

  /**
   * Amount of seconds between movement among two blocks.
   */
  var interval: Float = 1 //seconds to advance a single block

  var isDashing: Boolean = _
  val dashFactor: Float = 2f // two times faster, half interval

  // GameObjects
  lazy val terrain: Terrain = Terrain.random(terrainWidth, terrainHeight, 1-terrainSpaceDensity)

  lazy val snake: Snake = new Snake(terrain randTrail initialSnakeLength)
  lazy val berries: ListBuffer[Berry] = {
    val berries = new ListBuffer[Berry]
    1 to initialBerryCount foreach { _ =>  berries += createBerryAtEmpty(berries) }
    berries
  }


  var totalTime: Float = _
  private var accumulatedTime: Float = _
  def animationAlpha: Float = /*if (accumulatedTime > interval) 1f else*/ accumulatedTime / interval

  private var _gameState: GameState = Died

  def gameState:GameState = _gameState
  def gameState_=(newState: GameState) {
    if (gameState != newState) {
      _gameState = newState
      stateChanged(newState)
    }
  }

  def pause_=(b: Boolean) {
    if (gameState == Continues && b) {
      gameState = Paused
    } else if (gameState == Paused && !b) {
      gameState = Continues
    }
  }

  private var dir:Symbol = _

  def turnLeft() {
    if (dir != 'left) {
      snake.readyToTurnLeft()
      dir = 'left
    }
  }

  def turnRight() {
    if (dir != 'right) {
      snake.readyToTurnRight()
      dir = 'right
    }
  }

  def update(tpf: Float) {
    if (gameState == Continues) {
      totalTime += tpf
      accumulatedTime += tpf * (if (isDashing) dashFactor else 1)
      updateBerries(tpf)
      if (accumulatedTime > interval) {
        advance()
        accumulatedTime = 0
      }
    }
  }

  private def advance() {
    dir = null
    snake.move()
    checkDeath()
    if (gameState != Died) {
      snakePartMoved(snake)
      checkAteBerry()
    }
  }

  private def checkDeath() {
    //val partsThatCollide = snake.parts filter { part => (snake.sHead collides part) && part != snake.sHead }
    //val partsThatOut = snake.parts filter { part => !(terrain isOver part) }

    val anyCollisions = snake.sTail exists { snake.sHead collides _ }
    val anyOut = ! (terrain isOver snake.sHead) //snake.parts exists { part => !(terrain isOver part) }

    if (anyCollisions || anyOut) {
      gameState = Died
    }

  }

  var _score: Int = _
  def score: Int = _score
  def score_=(sc: Int) {
    _score = sc
    scoreChanged(sc)
  }

  private def checkAteBerry() {
    val newParts = berries filter { snake.sHead collides _ } map {  b =>
      score += b.score
      val part = snake.addPartToEnd()
      replaceBerry(b)
      interval *= diminishInterval
      if (interval < minInterval) interval = minInterval
      part
    }
    if (!newParts.isEmpty) {
      snakePartAdded(newParts)
    }
  }

  private def createBerryAtEmpty(berries: Seq[Berry] = berries): Berry = {
    var berry = Berry(terrain.randomNonSpace)
    while (snake.parts.exists(berry collides _) || berries.exists(berry collides _)) {
      berry = Berry(terrain.randomNonSpace)
    }
    berry
  }

  private def replaceBerry(b: Berry) {
    berries -= b
    val newB = createBerryAtEmpty()
    berries += newB
    berryRemoved(Seq(b))
    berryAdded(Seq(newB))
  }

  private def updateBerries(tpf: Float) {
    berries foreach { b =>
      b.update(tpf)
      if (b.shouldRemove) replaceBerry(b)
    }
  }

  private def snakePartAdded(parts: Seq[SnakePart]) {
    snakePartAddedHandlers foreach ( _(parts) )
  }

  private def snakePartMoved(snake: Snake) {
    snakePartMovedHandlers foreach ( _(snake) )
  }

  private def berryAdded(berries: Seq[Berry]) {
    berryAddedHandlers foreach ( _(berries) )
  }

  private def berryRemoved(berries: Seq[Berry]) {
    berryRemovedHandlers foreach ( _(berries) )
  }

  private def stateChanged(state: GameState) {
    gameStateHandlers foreach ( _(state) )
  }

  private def scoreChanged(score: Int) {
    scoreChangedHandlers foreach ( _(score) )
  }

}

