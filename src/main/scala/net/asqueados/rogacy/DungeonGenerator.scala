package net.asqueados.rogacy

import scala.util.Random
import scala.math.*

object DungeonGenerator {
  def generate(width: Int, height: Int): (LevelMap, Position, Position, Vector[Personaje]) = {
    val random = new Random()
    var grid = Vector.fill(height, width)('#')
    
    case class Room(x: Int, y: Int, w: Int, h: Int) {
      def centerX: Int = x + w / 2
      def centerY: Int = y + h / 2
      def intersects(other: Room): Boolean = {
        x < other.x + other.w && x + w > other.x &&
        y < other.y + other.h && y + h > other.y
      }
    }

    var rooms = Vector.empty[Room]
    val maxRooms = 15
    val minRoomSize = 5
    val maxRoomSize = 12

    for (_ <- 0 until 50) { // Try 50 times to place rooms
      if (rooms.size < maxRooms) {
        val w = random.nextInt(max(1, maxRoomSize - minRoomSize + 1)) + minRoomSize
        val h = random.nextInt(max(1, maxRoomSize - minRoomSize + 1)) + minRoomSize
        val xBound = width - w - 2
        val yBound = height - h - 2
        if (xBound >= 0 && yBound >= 0) {
          val x = random.nextInt(xBound + 1) + 1
          val y = random.nextInt(yBound + 1) + 1
          val newRoom = Room(x, y, w, h)
          
          if (!rooms.exists(r => r.intersects(newRoom.copy(x = newRoom.x - 1, y = newRoom.y - 1, w = newRoom.w + 2, h = newRoom.h + 2)))) {
          // Carve room
          for (ry <- y until y + h; rx <- x until x + w) {
            grid = grid.updated(ry, grid(ry).updated(rx, '.'))
          }
          
          if (rooms.nonEmpty) {
            val prev = rooms.last
            val cx1 = prev.centerX
            val cy1 = prev.centerY
            val cx2 = newRoom.centerX
            val cy2 = newRoom.centerY
            
            if (random.nextBoolean()) {
              carveH(cx1, cx2, cy1)
              carveV(cy1, cy2, cx2)
            } else {
              carveV(cy1, cy2, cx1)
              carveH(cx1, cx2, cy2)
            }
          }
          rooms = rooms :+ newRoom
        }
      }
    }

    def carveH(x1: Int, x2: Int, y: Int): Unit = {
      for (x <- min(x1, x2) to max(x1, x2)) {
        grid = grid.updated(y, grid(y).updated(x, '.'))
      }
    }

    def carveV(y1: Int, y2: Int, x: Int): Unit = {
      for (y <- min(y1, y2) to max(y1, y2)) {
        grid = grid.updated(y, grid(y).updated(x, '.'))
      }
    }

    // Add doors
    for (y <- 1 until height - 1; x <- 1 until width - 1) {
      if (grid(y)(x) == '.') {
        // Horizontal doorway: walls above and below, floor left and right
        if (grid(y - 1)(x) == '#' && grid(y + 1)(x) == '#' && grid(y)(x - 1) == '.' && grid(y)(x + 1) == '.') {
          if (random.nextInt(10) < 3) grid = grid.updated(y, grid(y).updated(x, '+'))
        }
        // Vertical doorway: walls left and right, floor above and below
        else if (grid(y)(x - 1) == '#' && grid(y)(x + 1) == '#' && grid(y - 1)(x) == '.' && grid(y + 1)(x) == '.') {
          if (random.nextInt(10) < 3) grid = grid.updated(y, grid(y).updated(x, '+'))
        }
      }
    }
    
    // Place stairs
    var upPos = Position(width/2, height/2)
    var downPos = Position(width/2, height/2)

    if (rooms.size >= 2) {
      val possiblePairs = for {
        r1 <- rooms
        r2 <- rooms
        if r1 != r2
        dist = abs(r1.centerX - r2.centerX)
        if dist >= width / 2
      } yield (r1, r2)
      
      val (roomUp, roomDown) = if (possiblePairs.nonEmpty) {
        possiblePairs(random.nextInt(possiblePairs.size))
      } else {
        rooms.flatMap(r1 => rooms.map(r2 => (r1, r2)))
          .maxBy { case (r1, r2) => abs(r1.centerX - r2.centerX) }
      }
      
      upPos = Position(roomUp.centerX, roomUp.centerY)
      downPos = Position(roomDown.centerX, roomDown.centerY)
      
      grid = grid.updated(upPos.y, grid(upPos.y).updated(upPos.x, '<'))
      grid = grid.updated(downPos.y, grid(downPos.y).updated(downPos.x, '>'))
    }
    
    // Place some entities
    val entities = if (rooms.size > 1) {
      rooms.drop(1).flatMap { room =>
        val numEntities = random.nextInt(2) // 0 or 1 entity per room
        (0 until numEntities).flatMap { _ =>
          val eX = room.x + random.nextInt(max(1, room.w))
          val eY = room.y + random.nextInt(max(1, room.h))
          val pos = Position(eX, eY)
          // Don't place entity on stairs
          if (grid(eY)(eX) == '.') {
            if (random.nextBoolean()) Some(Personaje("Goblin", 'g', pos, Colors.Green))
            else Some(Personaje("Troll", 'T', pos, Colors.Red))
          } else None
        }
      }
    } else Vector.empty[Personaje]

    (LevelMap(grid, width, height), upPos, downPos, entities)
  }
}
