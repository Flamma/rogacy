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
    newState.time shouldEqual 100
  }

  it should "correctly move player down to valid position and increase time" in {
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
    newState.time shouldEqual 100
  }

  it should "correctly move player left to valid position and increase time" in {
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
    newState.time shouldEqual 100
  }

  it should "correctly move player right to valid position and increase time" in {
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
    newState.time shouldEqual 100
  }

  it should "not move player into walls and not increase time" in {
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
    newState.time shouldEqual 0
  }

  it should "handle stairs correctly and increase time" in {
    val grid = Vector.fill(10, 20)('.')
    val withStairs = grid.updated(5, grid(5).updated(5, '>'))
    val gameMap = LevelMap(withStairs, 20, 10)
    val player = Player(Position(5, 5))
    val initialState = GameState(gameMap, player, Vector.empty, depth = 1)
    
    val newState = Game.handleInput(initialState, '>')
    newState.depth shouldEqual 2
    newState.messages.last should include ("level 2")
    newState.time shouldEqual 100
    // Map should have changed, and player should be at the up stairs of the new map
    newState.map should not be gameMap
    val tileAtNewPos = newState.map.getTile(newState.player.position.x, newState.player.position.y)
    tileAtNewPos shouldEqual '<'
  }

  it should "not move player into entities and increase time (due to attack)" in {
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
    newState.messages.last should include ("hit")
    newState.time shouldEqual 100
  }

  it should "substract HP when player hits an entity and remove it when HP reaches 0 and increase time" in {
    val grid = Vector.fill(10, 20)('.')
    val gameMap = LevelMap(grid, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector(Personaje("Goblin", 'g', Position(5, 4), hp = 2))
    val initialState = GameState(gameMap, player, entities)
    
    // First hit
    val stateAfterFirstHit = Game.movePlayer(initialState, 0, -1)
    stateAfterFirstHit.entities.head.hp shouldEqual 1
    stateAfterFirstHit.messages.last should include ("hit")
    stateAfterFirstHit.messages.last should include ("1 hp left")
    stateAfterFirstHit.time shouldEqual 100
    
    // Second hit
    val stateAfterSecondHit = Game.movePlayer(stateAfterFirstHit, 0, -1)
    stateAfterSecondHit.entities shouldBe empty
    stateAfterSecondHit.messages.last should include ("kill")
    stateAfterSecondHit.time shouldEqual 200
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
    newStateW.time shouldEqual 100
    
    // Test moving down
    val newStateS = Game.handleInput(initialState, 's')
    newStateS.player.position shouldEqual Position(5, 6)
    newStateS.time shouldEqual 100
    
    // Test moving left
    val newStateA = Game.handleInput(initialState, 'a')
    newStateA.player.position shouldEqual Position(4, 5)
    newStateA.time shouldEqual 100
    
    // Test moving right
    val newStateD = Game.handleInput(initialState, 'd')
    newStateD.player.position shouldEqual Position(6, 5)
    newStateD.time shouldEqual 100

    // Test numpad movement
    val newState8 = Game.handleInput(initialState, '8')
    newState8.player.position shouldEqual Position(5, 4)
    newState8.time shouldEqual 100

    val newState7 = Game.handleInput(initialState, '7')
    newState7.player.position shouldEqual Position(4, 4)
    newState7.time shouldEqual 100

    val newState9 = Game.handleInput(initialState, '9')
    newState9.player.position shouldEqual Position(6, 4)
    newState9.time shouldEqual 100

    val newState1 = Game.handleInput(initialState, '1')
    newState1.player.position shouldEqual Position(4, 6)
    newState1.time shouldEqual 100

    val newState3 = Game.handleInput(initialState, '3')
    newState3.player.position shouldEqual Position(6, 6)
    newState3.time shouldEqual 100
    
    // Test wait/pass turn
    val newStateWait = Game.handleInput(initialState, '5')
    newStateWait.time shouldEqual 100
    
    // Test quit
    val newStateQ = Game.handleInput(initialState, 'q')
    newStateQ.running shouldBe false
    newStateQ.time shouldEqual 0
    
    // Test invalid input
    val newStateInvalid = Game.handleInput(initialState, 'x')
    newStateInvalid shouldBe initialState
    newStateInvalid.time shouldEqual 0
  }

  "Monster AI" should "move monster toward player if visible" in {
    val grid = Vector.fill(10, 20)('.')
    val gameMap = LevelMap(grid, 20, 10)
    val player = Player(Position(5, 5))
    // Goblin at (7,5), speed 75. 
    // Player just moved and time is 100.
    val entities = Vector(Personaje("Goblin", 'g', Position(7, 5), speed = 75))
    val state = GameState(gameMap, player, entities, time = 100)
    
    val newState = Game.processMonsters(state)
    // Goblin should have moved closer. Both (6,5) and (6,4) are valid next steps toward (5,5).
    val validPositions = Set(Position(6, 5), Position(6, 4))
    validPositions should contain (newState.entities.head.position)
    // Goblin should have moved twice? nextActionTime: 0 -> 75 -> 150.
    // At T=100, 0 <= 100 (moves to 6,5, next=75), then 75 <= 100 (moves to 5,5 is target so attacks).
    // Wait, if it moves to (6,5) at T=75, then at T=150 it would attack.
    // Let's check the logic.
    // Initial: nextActionTime = 0.
    // T=100. 0 <= 100 is true.
    // Monster moves to (6,5), nextActionTime = 75.
    // Loop again. 75 <= 100 is true.
    // Monster is at (6,5), target is (5,5). It's adjacent, so it attacks. nextActionTime = 150.
    // Loop again. 150 <= 100 is false.
    newState.entities.head.nextActionTime shouldEqual 150
    newState.player.health shouldEqual 9
  }

  it should "stay still if player is not visible" in {
    val grid = Vector.fill(10, 20)('.')
    // Wall between player and monster
    val withWall = grid.updated(5, grid(5).updated(6, '#'))
    val gameMap = LevelMap(withWall, 20, 10)
    val player = Player(Position(5, 5))
    val entities = Vector(Personaje("Goblin", 'g', Position(7, 5), speed = 75))
    val state = GameState(gameMap, player, entities, time = 100)
    
    val newState = Game.processMonsters(state)
    newState.entities.head.position shouldEqual Position(7, 5)
    // Time should still be consumed to avoid infinite loop
    newState.entities.head.nextActionTime should be > 0L
  }
}
