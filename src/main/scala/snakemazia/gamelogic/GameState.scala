package snakemazia.gamelogic

/**
 * User: ceyhun
 * Date: 2/14/13
 * Time: 1:50 AM
 * 
 */
trait GameState
case object Continues extends GameState
case object Died extends GameState
case object Paused extends GameState


