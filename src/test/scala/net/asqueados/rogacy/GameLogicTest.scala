package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameLogicTest extends AnyFlatSpec with Matchers {
  "Game movement logic" should "correctly move player up" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector.empty[Personaje]  // No entities for this test
    val initialState = GameState(gameMap, player, entities)
    
    val newState = Game.movePlayer(initialState, 0, -1)
    newState.player.position shouldEqual Position(5, 4)
  }

  it should "correctly move player down to valid position" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector.empty[Personaje]  // No entities for this test
    val initialState = GameState(gameMap, player, entities)
    
    // Test moving down to (5,6) which should be walkable
    val newState = Game.movePlayer(initialState, 0, 1)
    newState.player.position shouldEqual Position(5, 6)
  }

  it should "correctly move player left to valid position" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector.empty[Personaje]  // No entities for this test
    val initialState = GameState(gameMap, player, entities)
    
    // Test moving left to (4,5) which should be walkable
    val newState = Game.movePlayer(initialState, -1, 0)
    newState.player.position shouldEqual Position(4, 5)
  }

  it should "correctly move player right to valid position" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector.empty[Personaje]  // No entities for this test
    val initialState = GameState(gameMap, player, entities)
    
    // Test moving right to (6,5) which should be walkable
    val newState = Game.movePlayer(initialState, 1, 0)
    newState.player.position shouldEqual Position(6, 5)
  }

  it should "not move player into walls" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9 || (x == 5 && y == 4)) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector.empty[Personaje]  // No entities for this test
    val initialState = GameState(gameMap, player, entities)
    
    val newState = Game.movePlayer(initialState, 0, -1)
    newState.player.position shouldEqual Position(5, 5)
  }

  it should "handle stairs correctly" in {
    val grid = Vector.fill(10, 20)('.')
    val withStairs = grid.updated(5, grid(5).updated(5, '>'))
    val gameMap = LevelMap(withStairs, 20, 10)
    val player = Player(Position(5, 5))
    val initialState = GameState(gameMap, player, Vector.empty, depth = 1)
    
    val newState = Game.handleInput(initialState, '>')
    newState.depth shouldEqual 2
    newState.messages.last should include ("You descend to level 2.")
    // Map should have changed, and player should be at the up stairs of the new map
    newState.map should not be gameMap
    val tileAtNewPos = newState.map.getTile(newState.player.position.x, newState.player.position.y)
    tileAtNewPos shouldEqual '<'
  }

  it should "not move player into entities" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector(Personaje("Goblin", 'g', Position(5, 4)))  // Entity at target position
    val initialState = GameState(gameMap, player, entities)
    
    val newState = Game.movePlayer(initialState, 0, -1)
    newState.player.position shouldEqual Position(5, 5)
    newState.messages.last should include ("You bump into the goblin!")
  }

  it should "handle input correctly for movement" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = LevelMap(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector(Personaje("Goblin", 'g', Position(2, 2)))
    val initialState = GameState(gameMap, player, entities)
    
    // Test moving up
    val newStateW = Game.handleInput(initialState, 'w')
    newStateW.player.position shouldEqual Position(5, 4)
    
    // Test moving down
    val newStateS = Game.handleInput(initialState, 's')
    newStateS.player.position shouldEqual Position(5, 6)
    
    // Test moving left
    val newStateA = Game.handleInput(initialState, 'a')
    newStateA.player.position shouldEqual Position(4, 5)
    
    // Test moving right
    val newStateD = Game.handleInput(initialState, 'd')
    newStateD.player.position shouldEqual Position(6, 5)
    
    // Test quit
    val newStateQ = Game.handleInput(initialState, 'q')
    newStateQ.running shouldBe false
    
    // Test invalid input
    val newStateInvalid = Game.handleInput(initialState, 'x')
    newStateInvalid shouldBe initialState
  }
}
