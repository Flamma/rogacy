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
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    
    val movedPlayer = Game.movePlayer(player, 0, -1, gameMap)
    movedPlayer shouldBe defined
    movedPlayer.get.position shouldEqual Position(5, 4)
  }

  it should "correctly move player down to valid position" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    
    // Test moving down to (5,6) which should be walkable
    val movedPlayer = Game.movePlayer(player, 0, 1, gameMap)
    movedPlayer shouldBe defined
    movedPlayer.get.position shouldEqual Position(5, 6)
  }

  it should "correctly move player left to valid position" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    
    // Test moving left to (4,5) which should be walkable
    val movedPlayer = Game.movePlayer(player, -1, 0, gameMap)
    movedPlayer shouldBe defined
    movedPlayer.get.position shouldEqual Position(4, 5)
  }

  it should "correctly move player right to valid position" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    
    // Test moving right to (6,5) which should be walkable
    val movedPlayer = Game.movePlayer(player, 1, 0, gameMap)
    movedPlayer shouldBe defined
    movedPlayer.get.position shouldEqual Position(6, 5)
  }

  it should "not move player into walls" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9 || (x == 5 && y == 4)) '#'
        else cell
      }
    }
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    
    val movedPlayer = Game.movePlayer(player, 0, -1, gameMap)
    movedPlayer shouldBe None
  }

  it should "handle input correctly for movement" in {
    val grid = Vector.fill(10, 20)('.')
    val withWalls = grid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == 19 || y == 0 || y == 9) '#'
        else cell
      }
    }
    val gameMap = Map(withWalls, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector(Entity(Position(2, 2), 'g'))
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