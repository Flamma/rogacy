package net.asqueados.rogacy

import scala.util.Random

object DungeonGenerator {
  def generate(width: Int, height: Int): (LevelMap, Position, Position, Vector[Personaje]) = {
    val random = new scala.util.Random()
    var grid = scala.collection.immutable.Vector.fill(height, width)('#')
    
    case class Room(x: Int, y: Int, w: Int, h: Int) {
      def centerX: Int = x + w / 2
      def centerY: Int = y + h / 2
      def intersects(other: Room): Boolean = {
        x < other.x + other.w && x + w > other.x &&
        y < other.y + other.h && y + h > other.y
      }
    }

    def carveH(x1: Int, x2: Int, y: Int, currentGrid: Vector[Vector[Char]]): Vector[Vector[Char]] = {
      var g = currentGrid
      for (x <- scala.math.min(x1, x2) to scala.math.max(x1, x2)) {
        g = g.updated(y, g(y).updated(x, '.'))
      }
      g
    }

    def carveV(y1: Int, y2: Int, x: Int, currentGrid: Vector[Vector[Char]]): Vector[Vector[Char]] = {
      var g = currentGrid
      for (y <- scala.math.min(y1, y2) to scala.math.max(y1, y2)) {
        g = g.updated(y, g(y).updated(x, '.'))
      }
      g
    }

    var rooms = scala.collection.immutable.Vector.empty[Room]
    val maxRooms = (width * height) / 500
    val minRoomSize = 3
    val maxRoomSize = 12

    for (_ <- 0 until width) { // More attempts proportional to width
      if (rooms.size < maxRooms) {
        val w = random.nextInt(scala.math.max(1, maxRoomSize - minRoomSize + 1)) + minRoomSize
        val h = random.nextInt(scala.math.max(1, maxRoomSize - minRoomSize + 1)) + minRoomSize
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
                grid = carveH(cx1, cx2, cy1, grid)
                grid = carveV(cy1, cy2, cx2, grid)
              } else {
                grid = carveV(cy1, cy2, cx1, grid)
                grid = carveH(cx1, cx2, cy2, grid)
              }
            }
            rooms = rooms :+ newRoom
          }
        }
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
    var upPos = Position(1, 1)
    var downPos = Position(width - 2, height - 2)

    if (rooms.size >= 2) {
      val possiblePairs = for {
        r1 <- rooms
        r2 <- rooms
        if r1 != r2
        dist = scala.math.abs(r1.centerX - r2.centerX)
        if dist >= width / 2
      } yield (r1, r2)
      
      val (roomUp, roomDown) = if (possiblePairs.nonEmpty) {
        possiblePairs(random.nextInt(possiblePairs.size))
      } else {
        rooms.flatMap(r1 => rooms.map(r2 => (r1, r2)))
          .maxBy { case (r1, r2) => scala.math.abs(r1.centerX - r2.centerX) }
      }
      upPos = Position(roomUp.centerX, roomUp.centerY)
      downPos = Position(roomDown.centerX, roomDown.centerY)
    } else if (rooms.size == 1) {
      val r = rooms.head
      upPos = Position(r.x, r.y)
      downPos = Position(r.x + r.w - 1, r.y + r.h - 1)
    } else {
      // Emergency floor carving if no rooms
      grid = grid.updated(upPos.y, grid(upPos.y).updated(upPos.x, '.'))
      grid = grid.updated(downPos.y, grid(downPos.y).updated(downPos.x, '.'))
    }
    
    // Always force stairs onto the grid
    grid = grid.updated(upPos.y, grid(upPos.y).updated(upPos.x, '<'))
    grid = grid.updated(downPos.y, grid(downPos.y).updated(downPos.x, '>'))
    
    // Place some entities
    val entities = if (rooms.size > 1) {
      rooms.drop(1).flatMap { room =>
        val numEntities = random.nextInt(2) // 0 or 1 entity per room
        (0 until numEntities).flatMap { _ =>
          val eX = room.x + random.nextInt(scala.math.max(1, room.w))
          val eY = room.y + random.nextInt(scala.math.max(1, room.h))
          val pos = Position(eX, eY)
          // Don't place entity on stairs
          if (grid(eY)(eX) == '.') {
            if (random.nextBoolean()) Some(Personaje("Goblin", 'g', pos, Colors.Green, hp = 3, speed = 75))
            else Some(Personaje("Troll", 'T', pos, Colors.Red, hp = 3, speed = 150))
          } else None
        }
      }
    } else scala.collection.immutable.Vector.empty[Personaje]

    (LevelMap(grid, width, height), upPos, downPos, entities)
  }
}
