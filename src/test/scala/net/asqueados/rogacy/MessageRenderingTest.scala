package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MessageRenderingTest extends AnyFlatSpec with Matchers {
  "Message rendering" should "handle pagination correctly" in {
    val grid = Vector.fill(10, 20)('.')
    val map = LevelMap(grid, 20, 10)
    val player = Player(Position(1, 1))
    val entities = Vector.empty[Personaje]
    val gameState = GameState(map, player, entities, true, Vector("This is a very long message that exceeds the width of the map"), 0)
    
    // Test that the message is longer than the map width
    gameState.messages.last.length should be > gameState.map.width
    
    // Test pagination calculation
    val maxMessageLength = gameState.map.width
    val messageLength = gameState.messages.last.length
    val totalPages = (messageLength + maxMessageLength - 1) / maxMessageLength
    
    totalPages should be > 1
  }
  
  "GameState" should "add messages correctly" in {
    val grid = Vector.fill(10, 20)('.')
    val map = LevelMap(grid, 20, 10)
    val player = Player(Position(1, 1))
    val entities = Vector.empty[Personaje]
    val initialGameState = GameState(map, player, entities)
    
    // Test adding a message
    val newGameState = initialGameState.addMessage("Test message")
    
    // Verify the message was added
    newGameState.messages should contain("Test message")
    newGameState.messages.length should be > initialGameState.messages.length
    
    // Verify pagination is reset
    newGameState.currentMessagePage should be(0)
  }
  
  "Entity interaction" should "generate messages when player bumps into entities" in {
    val grid = Vector.fill(10, 20)('.')
    val map = LevelMap(grid, 20, 10)
    val player = Player(Position(1, 1))
    val entities = Vector(Personaje("Goblin", 'g', Position(2, 1))) // Entity at position (2,1)
    
    // Test that player can move to position (2,1) but will get interaction message
    val (playerOption, messageOption) = Game.movePlayer(player, 1, 0, map, entities)
    
    // Player should not move (None) but should get interaction message (Some)
    playerOption should be(None)
    messageOption should not be None
    messageOption.get should include("goblin")
  }
}