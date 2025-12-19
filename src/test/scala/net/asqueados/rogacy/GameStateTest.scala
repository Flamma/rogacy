package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameStateTest extends AnyFlatSpec with Matchers {
  "A GameState" should "correctly initialize with all components" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(1, 1))
    val entities = Vector(Entity(Position(5, 5), 'g'))
    val gameState = GameState(gameMap, player, entities, true)
    
    gameState.map shouldEqual gameMap
    gameState.player shouldEqual player
    gameState.entities shouldEqual entities
    gameState.running shouldBe true
  }

  it should "be immutable" in {
    val grid = Vector.fill(10, 20)('.')
    val gameMap = Map(grid, 20, 10)
    val player = Player(Position(0, 0))
    val entities = Vector.empty[Entity]
    val gameState = GameState(gameMap, player, entities)
    
    // This would cause compilation error if GameState was mutable
    // gameState.running = false // This line would not compile
  }
}