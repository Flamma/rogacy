package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LevelMapTest extends AnyFlatSpec with Matchers {
  "A LevelMap" should "correctly initialize with grid dimensions" in {
    val grid = Vector.fill(10, 20)('.')
    val gameMap = LevelMap(grid, 20, 10)
    
    gameMap.width shouldEqual 20
    gameMap.height shouldEqual 10
  }

  it should "correctly identify walkable tiles" in {
    val grid = Vector.fill(10, 20)('.')
    val gameMap = LevelMap(grid, 20, 10)
    
    // Floor and stairs should be walkable
    gameMap.isWalkable(1, 1) shouldBe true
    
    val withStairs = grid.updated(2, grid(2).updated(2, '<')).updated(3, grid(3).updated(3, '>'))
    val mapWithStairs = LevelMap(withStairs, 20, 10)
    mapWithStairs.isWalkable(2, 2) shouldBe true
    mapWithStairs.isWalkable(3, 3) shouldBe true
  }

  it should "correctly identify non-walkable tiles" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.updated(1, grid(1).updated(1, '#'))
    val gameMap = LevelMap(withWalls, 20, 10)
    
    // Wall should not be walkable
    gameMap.isWalkable(1, 1) shouldBe false
  }

  it should "handle boundary conditions correctly" in {
    val grid = Vector.fill(10, 20)('.')
    val gameMap = LevelMap(grid, 20, 10)
    
    // Out of bounds should be treated as walls
    gameMap.isWalkable(-1, -1) shouldBe false
    gameMap.isWalkable(25, 25) shouldBe false
  }

  it should "render correctly with player and entities" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(1, 1))
    val entities = Vector(Personaje("Goblin", 'g', Position(5, 5)))
    
    val rendered = gameMap.render(player, entities)
    rendered should not be empty
    rendered should include("@")
    rendered should include("#")
    rendered should include("g")
  }

  it should "render with colors when enabled" in {
    val grid = Vector.fill(5, 5)('.')
    val gameMap = LevelMap(grid, 5, 5)
    val player = Player(Position(1, 1), color = Colors.Green)
    val entities = Vector(Personaje("Goblin", 'g', Position(3, 3), color = Colors.Red))
    
    val rendered = gameMap.render(player, entities, colorsEnabled = true)
    rendered should include(Colors.Green + "@" + Colors.Reset)
    rendered should include(Colors.Red + "g" + Colors.Reset)
  }
}