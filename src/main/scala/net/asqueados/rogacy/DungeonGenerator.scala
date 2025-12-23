package net.asqueados.rogacy

import scala.util.Random

object DungeonGenerator {
  def generate(width: Int, height: Int): (LevelMap, Player, Vector[Personaje]) = {
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
        val w = random.nextInt(maxRoomSize - minRoomSize + 1) + minRoomSize
        val h = random.nextInt(maxRoomSize - minRoomSize + 1) + minRoomSize
        val x = random.nextInt(width - w - 2) + 1
        val y = random.nextInt(height - h - 2) + 1
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
      for (x <- math.min(x1, x2) to math.max(x1, x2)) {
        grid = grid.updated(y, grid(y).updated(x, '.'))
      }
    }

    def carveV(y1: Int, y2: Int, x: Int): Unit = {
      for (y <- math.min(y1, y2) to math.max(y1, y2)) {
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
    
    // Place player in the first room
    val playerPos = if (rooms.nonEmpty) Position(rooms.head.centerX, rooms.head.centerY) else Position(width/2, height/2)

    // Place stairs
    if (rooms.size >= 2) {
      val possiblePairs = for {
        r1 <- rooms
        r2 <- rooms
        if r1 != r2
        dist = math.abs(r1.centerX - r2.centerX) // "at least half the dungeon width" suggests horizontal distance
        if dist >= width / 2
      } yield (r1, r2)
      
      val (roomUp, roomDown) = if (possiblePairs.nonEmpty) {
        possiblePairs(random.nextInt(possiblePairs.size))
      } else {
        // Fallback: pick rooms with maximum horizontal distance
        rooms.flatMap(r1 => rooms.map(r2 => (r1, r2)))
          .maxBy { case (r1, r2) => math.abs(r1.centerX - r2.centerX) }
      }
      
      grid = grid.updated(roomUp.centerY, grid(roomUp.centerY).updated(roomUp.centerX, '<'))
      grid = grid.updated(roomDown.centerY, grid(roomDown.centerY).updated(roomDown.centerX, '>'))
    }
    
    // Place some entities
    val entities = rooms.drop(1).flatMap { room =>
      val numEntities = random.nextInt(2) // 0 or 1 entity per room
      (0 until numEntities).flatMap { _ =>
        val eX = room.x + random.nextInt(room.w)
        val eY = room.y + random.nextInt(room.h)
        val pos = Position(eX, eY)
        // Don't place entity on stairs or player
        if (grid(eY)(eX) == '.' && pos != playerPos) {
          if (random.nextBoolean()) Some(Personaje("Goblin", 'g', pos, Colors.Green))
          else Some(Personaje("Troll", 'T', pos, Colors.Red))
        } else None
      }
    }

    (LevelMap(grid, width, height), Player(playerPos), entities)
  }
}
