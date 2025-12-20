package net.asqueados.rogacy

case class Position(x: Int, y: Int)
case class Entity(position: Position, char: Char)
case class Player(position: Position, health: Int = 100)
case class LevelMap(grid: Vector[Vector[Char]], width: Int, height: Int) {
  def getTile(x: Int, y: Int): Char = {
    if (x >= 0 && x < width && y >= 0 && y < height) grid(y)(x) else '#'
  }

  def isWalkable(x: Int, y: Int, entities: Vector[Entity] = Vector.empty): Boolean = {
    if (getTile(x, y) == '#') false
    else {
      // Check if there's an entity at this position
      !entities.exists(_.position == Position(x, y))
    }
  }

  def render(player: Player, entities: Vector[Entity]): String = {
    val gridWithEntities = entities.foldLeft(grid) { (g, e) =>
      g.updated(e.position.y, g(e.position.y).updated(e.position.x, e.char))
    }
    val finalGrid = gridWithEntities.updated(player.position.y, gridWithEntities(player.position.y).updated(player.position.x, '@'))
    finalGrid.map(_.mkString).mkString("\n")
  }
}

case class GameState(map: LevelMap, player: Player, entities: Vector[Entity], running: Boolean = true)