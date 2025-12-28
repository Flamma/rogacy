package net.asqueados.rogacy

import scala.annotation.tailrec

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
    (tile == '.' || tile == '\'' || tile == '<' || tile == '>') && !entityAtPosition
  }

  def isOpaque(x: Int, y: Int): Boolean = {
    val tile = getTile(x, y)
    tile == '#' || tile == '+'
  }

  def hasLineOfSight(x0: Int, y0: Int, x1: Int, y1: Int): Boolean = {
    val dx = math.abs(x1 - x0)
    val dy = math.abs(y1 - y0)
    val sx = if (x0 < x1) 1 else -1
    val sy = if (y0 < y1) 1 else -1
    
    @tailrec
    def check(x: Int, y: Int, err: Int): Boolean = {
      if (x == x1 && y == y1) true
      else if ((x != x0 || y != y0) && isOpaque(x, y)) false
      else {
        val e2 = 2 * err
        var nextX = x
        var nextY = y
        var nextErr = err
        if (e2 > -dy) {
          nextErr -= dy
          nextX += sx
        }
        if (e2 < dx) {
          nextErr += dx
          nextY += sy
        }
        check(nextX, nextY, nextErr)
      }
    }
    check(x0, y0, dx - dy)
  }

  def render(player: Player, entities: Vector[Personaje], colorsEnabled: Boolean = false, viewportWidth: Int = 80, viewportHeight: Int = 20, viewEverything: Boolean = false): String = {
    val startX = scala.math.max(0, scala.math.min(width - viewportWidth, player.position.x - viewportWidth / 2))
    val startY = scala.math.max(0, scala.math.min(height - viewportHeight, player.position.y - viewportHeight / 2))
    
    val sb = new StringBuilder()
    for (y <- startY until scala.math.min(startY + viewportHeight, height)) {
      for (x <- startX until scala.math.min(startX + viewportWidth, width)) {
        val pos = Position(x, y)
        val isVisible = viewEverything || hasLineOfSight(player.position.x, player.position.y, x, y)
        
        if (!isVisible) {
          sb.append(' ')
        } else if (player.position == pos) {
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
      if (y < startY + viewportHeight - 1 && y < height - 1) sb.append('\n')
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
  currentMessagePage: Int = 0,
  depth: Int = 1,
  viewEverything: Boolean = false
) {
  def addMessage(message: String): GameState = {
    this.copy(messages = messages :+ message, currentMessagePage = 0)
  }
}