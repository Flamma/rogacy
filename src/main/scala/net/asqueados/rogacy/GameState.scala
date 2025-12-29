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
  val Gray = "\u001b[90m"
}

case class Position(x: Int, y: Int)
case class Personaje(
  name: String, 
  symbol: Char, 
  position: Position, 
  color: String = Colors.White, 
  hp: Int = 3,
  speed: Int = 100,
  nextActionTime: Long = 0L
) {
  def interact(): String = s"You attack the $coloredName!"
  def coloredName: String = s"$color${name.toLowerCase}${Colors.Reset}"
}
case class Player(position: Position, health: Int = 10, color: String = Colors.BrightWhite)
case class LevelMap(grid: Vector[Vector[Char]], width: Int, height: Int, explored: Vector[Vector[Boolean]]) {
  def getTile(x: Int, y: Int): Char = {
    if (x >= 0 && x < width && y >= 0 && y < height) grid(y)(x) else '#'
  }

  def isExplored(x: Int, y: Int): Boolean = {
    if (x >= 0 && x < width && y >= 0 && y < height) explored(y)(x) else false
  }

  def updateTile(x: Int, y: Int, tile: Char): LevelMap = {
    if (x >= 0 && x < width && y >= 0 && y < height) {
      val newGrid = grid.updated(y, grid(y).updated(x, tile))
      this.copy(grid = newGrid)
    } else this
  }

  def markExplored(x: Int, y: Int): LevelMap = {
    if (x >= 0 && x < width && y >= 0 && y < height && !explored(y)(x)) {
      val newExplored = explored.updated(y, explored(y).updated(x, true))
      this.copy(explored = newExplored)
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
        val explored = isExplored(x, y)
        
        if (!isVisible && !explored) {
          sb.append(' ')
        } else if (isVisible && player.position == pos) {
          if (colorsEnabled) sb.append(player.color).append('@').append(Colors.Reset)
          else sb.append('@')
        } else {
          val entity = if (isVisible) entities.find(_.position == pos) else None
          entity match {
            case Some(e) =>
              if (colorsEnabled) sb.append(e.color).append(e.symbol).append(Colors.Reset)
              else sb.append(e.symbol)
            case None =>
              val tile = grid(y)(x)
              if (colorsEnabled) {
                val color = if (isVisible) {
                  if (tile == '+' || tile == '\'') Colors.Brown
                  else Colors.White
                } else {
                  Colors.Gray
                }
                sb.append(color).append(tile).append(Colors.Reset)
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

object LevelMap {
  def apply(grid: Vector[Vector[Char]], width: Int, height: Int): LevelMap = {
    LevelMap(grid, width, height, Vector.fill(height, width)(false))
  }
}

object Constants {
  val ViewportWidth = 80
  val ViewportHeight = 20
}

case class GameState(
  map: LevelMap, 
  player: Player, 
  entities: Vector[Personaje], 
  running: Boolean = true,
  messages: Vector[String] = Vector("Welcome to Rogacy!"),
  currentMessageIndex: Int = 0,
  currentMessagePage: Int = 0,
  depth: Int = 1,
  viewEverything: Boolean = false,
  time: Long = 0L
) {
  def addMessage(message: String): GameState = {
    if (messages.isEmpty) {
      this.copy(messages = Vector(message))
    } else {
      val lastMsg = messages.last
      if (lastMsg.length + message.length + 2 <= Constants.ViewportWidth) {
        this.copy(messages = messages.init :+ (lastMsg + "  " + message))
      } else {
        this.copy(messages = messages :+ message)
      }
    }
  }

  def updateVisibility(): GameState = {
    val viewportWidth = Constants.ViewportWidth
    val viewportHeight = Constants.ViewportHeight
    val startX = scala.math.max(0, scala.math.min(map.width - viewportWidth, player.position.x - viewportWidth / 2))
    val startY = scala.math.max(0, scala.math.min(map.height - viewportHeight, player.position.y - viewportHeight / 2))
    
    var newMap = map
    for (y <- startY until scala.math.min(startY + viewportHeight, map.height)) {
      for (x <- startX until scala.math.min(startX + viewportWidth, map.width)) {
        if (map.hasLineOfSight(player.position.x, player.position.y, x, y)) {
          newMap = newMap.markExplored(x, y)
        }
      }
    }
    this.copy(map = newMap)
  }
}
