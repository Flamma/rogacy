package net.asqueados.rogacy

case class Position(x: Int, y: Int)
case class Personaje(name: String, symbol: Char, position: Position) {
  def interact(): String = s"You bump into the ${name.toLowerCase}!"
}
case class Player(position: Position, health: Int = 100)
case class LevelMap(grid: Vector[Vector[Char]], width: Int, height: Int) {
  def getTile(x: Int, y: Int): Char = {
    if (x >= 0 && x < width && y >= 0 && y < height) grid(y)(x) else '#'
  }

  def isWalkable(x: Int, y: Int, entities: Vector[Personaje] = Vector.empty): Boolean = {
    // Check if the tile itself is walkable (not a wall)
    // Check if there's an entity at this position
    val entityAtPosition = entities.exists(entity => entity.position == Position(x, y))
    getTile(x, y) != '#' && !entityAtPosition
  }

  def render(player: Player, entities: Vector[Personaje]): String = {
    val gridWithEntities = entities.foldLeft(grid) { (g, e) =>
      g.updated(e.position.y, g(e.position.y).updated(e.position.x, e.symbol))
    }
    val finalGrid = gridWithEntities.updated(player.position.y, gridWithEntities(player.position.y).updated(player.position.x, '@'))
    finalGrid.map(_.mkString).mkString("\n")
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