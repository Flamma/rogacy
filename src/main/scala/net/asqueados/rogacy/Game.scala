package net.asqueados.rogacy

import scala.annotation.tailrec
import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder

object Game {
  private val terminal = TerminalBuilder.builder().system(true).build()
  terminal.enterRawMode()
  private val inputReader = terminal.reader()
  def createInitialMap(): Map = {
    val width = 20
    val height = 10
    val baseGrid = Vector.fill(height, width)('.')
    val withWalls = baseGrid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == width - 1 || y == 0 || y == height - 1) '#'
        else if (x >= 5 && x <= 14 && y == 5) '#'
        else if (y >= 5 && y < height && x == 5) '#'
        else cell
      }
    }
    Map(withWalls, width, height)
  }

  def createInitialEntities(): Vector[Entity] = {
    Vector(
      Entity(Position(5, 3), 'g'),  // goblin
      Entity(Position(10, 6), 'p')  // potion
    )
  }

  def movePlayer(player: Player, dx: Int, dy: Int, map: Map): Option[Player] = {
    val newPos = Position(player.position.x + dx, player.position.y + dy)
    if (map.isWalkable(newPos.x, newPos.y)) Some(player.copy(position = newPos)) else None
  }

  def handleInput(state: GameState, input: Char): GameState = input match {
    case 'w' => state.copy(player = movePlayer(state.player, 0, -1, state.map).getOrElse(state.player))
    case 's' => state.copy(player = movePlayer(state.player, 0, 1, state.map).getOrElse(state.player))
    case 'a' => state.copy(player = movePlayer(state.player, -1, 0, state.map).getOrElse(state.player))
    case 'd' => state.copy(player = movePlayer(state.player, 1, 0, state.map).getOrElse(state.player))
    case 'q' => state.copy(running = false)
    case _ => state
  }

  @tailrec
  def loop(state: GameState): Unit = {
    println(state.map.render(state.player, state.entities))
    println("Move with WASD, Q to quit:")
    val input = inputReader.read().toChar.toLower
    val newState = handleInput(state, input)
    if (newState.running) loop(newState)
  }

  def start(): Unit = {
    val map = createInitialMap()
    val player = Player(Position(1, 1))
    val entities = createInitialEntities()
    val initialState = GameState(map, player, entities)
    loop(initialState)
  }
}