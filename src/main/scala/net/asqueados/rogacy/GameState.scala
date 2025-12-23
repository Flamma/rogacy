package net.asqueados.rogacy

object Colors {
  val Reset = "\u001b[0m"
  val Red = "\u001b[31m"
  val Green = "\u001b[32m"
  val Yellow = "\u001b[33m"
  val Blue = "\u001b[34m"
  val Magenta = "\u001b[35m"
  val Cyan = "\u001b[36m"
  val White = "\u001b[37m"
  val BrightWhite = "\u001b[97m"
  val Brown = "\u001b[33m"
}

case class Position(x: Int, y: Int)
case class Personaje(name: String, symbol: Char, position: Position, color: String = Colors.White) {
  def interact(): String = s"You bump into the $coloredName!"
  def coloredName: String = s"$color${name.toLowerCase}${Colors.Reset}"
}
case class Player(position: Position, health: Int = 100, color: String = Colors.BrightWhite)
case class LevelMap(grid: Vector[Vector[Char]], width: Int, height: Int) {
  def getTile(x: Int, y: Int): Char = {
    if (x >= 0 && x < width && y >= 0 && y < height) grid(y)(x) else '#'
  }

  def updateTile(x: Int, y: Int, tile: Char): LevelMap = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      val newGrid = grid.updated(y, grid(y).updated(x, tile))
      this.copy(grid = newGrid)
    } else this
  }

  def isWalkable(x: Int, y: Int, entities: Vector[Personaje] = Vector.empty): Boolean = {
    val tile = getTile(x, y)
    val entityAtPosition = entities.exists(entity => entity.position == Position(x, y))
    (tile == '.' || tile == '\'') && !entityAtPosition
  }

  def render(player: Player, entities: Vector[Personaje], colorsEnabled: Boolean = false): String = {
    val sb = new StringBuilder()
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val pos = Position(x, y)
        if (player.position == pos) {
          if (colorsEnabled) sb.append(player.color).append('@').append(Colors.Reset)
          else sb.append('@')
        } else {
          entities.find(_.position == pos) match {
            case Some(e) =>
              if (colorsEnabled) sb.append(e.color).append(e.symbol).append(Colors.Reset)
              else sb.append(e.symbol)
            case None =>
              val tile = grid(y)(x)
              if (colorsEnabled && (tile == '+' || tile == '\'')) {
                sb.append(Colors.Brown).append(tile).append(Colors.Reset)
              } else {
                sb.append(tile)
              }
          }
        }
      }
      if (y < height - 1) sb.append('\n')
    }
    sb.toString()
  }
}

case class GameState(
  map: LevelMap, 
  player: Player, 
  entities: Vector[Personaje], 
  running: Boolean = true,
  messages: Vector[String] = Vector("Welcome to Rogacy!"),
  currentMessagePage: Int = 0
) {
  def addMessage(message: String): GameState = {
    this.copy(messages = messages :+ message, currentMessagePage = 0)
  }
}