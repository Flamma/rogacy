package net.asqueados.rogacy

import scala.annotation.tailrec
import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder

object Game {
  private val terminal = TerminalBuilder.builder().system(true).build()
  terminal.enterRawMode()
  private val inputReader = terminal.reader()
  def createInitialMap(): LevelMap = {
    val width = 80
    val height = 20
    val baseGrid = Vector.fill(height, width)('.')
    val withWalls = baseGrid.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (cell, x) =>
        if (x == 0 || x == width - 1 || y == 0 || y == height - 1) '#'
        else if (x >= 5 && x <= 14 && y == 5) '#'
        else if (y >= 5 && y < height && x == 5) '#'
        else cell
      }
    }
    LevelMap(withWalls, width, height)
  }

  def createInitialEntities(): Vector[Entity] = {
    Vector(
      Entity(Position(5, 3), 'g'),  // goblin
      Entity(Position(10, 6), 'p')  // potion
    )
  }

  def movePlayer(
    player: Player, 
    dx: Int, 
    dy: Int, 
    map: LevelMap, 
    entities: Vector[Entity]
  ): (Option[Player], Option[String]) = {
    val newPos = Position(player.position.x + dx, player.position.y + dy)
    
    // Check if there's an entity at the new position
    entities.find(_.position == newPos) match {
      case Some(entity) =>
        // Player bumped into an entity, create interaction message
        val interactionMessage = entity.char match {
          case 'g' => "You bump into the goblin!"
          case 'p' => "You bump into the potion!"
          case c => s"You bump into the $c!"
        }
        (None, Some(interactionMessage)) // Player doesn't move, but there's a message
      case None =>
        // No entity at the new position, check if it's walkable
        if (map.isWalkable(newPos.x, newPos.y)) {
          (Some(player.copy(position = newPos)), None) // Player moves
        } else {
          (None, None) // Player doesn't move, no message
        }
    }
  }

  def handleInput(state: GameState, input: Char): GameState = input match {
    case 'w' => {
      val (playerOption, messageOption) = movePlayer(state.player, 0, -1, state.map, state.entities)
      var newState = state
      
      // Handle player movement
      if (playerOption.isDefined) {
        newState = newState.copy(player = playerOption.get)
        newState = newState.addMessage("You moved up.")
      }
      
      // Handle interaction message
      if (messageOption.isDefined) {
        newState = newState.addMessage(messageOption.get)
      }
      
      newState
    }
    case 's' => {
      val (playerOption, messageOption) = movePlayer(state.player, 0, 1, state.map, state.entities)
      var newState = state
      
      // Handle player movement
      if (playerOption.isDefined) {
        newState = newState.copy(player = playerOption.get)
        newState = newState.addMessage("You moved down.")
      }
      
      // Handle interaction message
      if (messageOption.isDefined) {
        newState = newState.addMessage(messageOption.get)
      }
      
      newState
    }
    case 'a' => {
      val (playerOption, messageOption) = movePlayer(state.player, -1, 0, state.map, state.entities)
      var newState = state
      
      // Handle player movement
      if (playerOption.isDefined) {
        newState = newState.copy(player = playerOption.get)
        newState = newState.addMessage("You moved left.")
      }
      
      // Handle interaction message
      if (messageOption.isDefined) {
        newState = newState.addMessage(messageOption.get)
      }
      
      newState
    }
    case 'd' => {
      val (playerOption, messageOption) = movePlayer(state.player, 1, 0, state.map, state.entities)
      var newState = state
      
      // Handle player movement
      if (playerOption.isDefined) {
        newState = newState.copy(player = playerOption.get)
        newState = newState.addMessage("You moved right.")
      }
      
      // Handle interaction message
      if (messageOption.isDefined) {
        newState = newState.addMessage(messageOption.get)
      }
      
      newState
    }
    case ' ' => handleMessagePagination(state)
    case 'q' => state.copy(running = false)
    case _ => state
  }
  
  def handleMessagePagination(state: GameState): GameState = {
    val maxMessageLength = state.map.width
    val currentMessages = state.messages
    val currentMessage = if (currentMessages.nonEmpty) currentMessages.last else ""
    
    if (currentMessage.length > maxMessageLength) {
      val totalPages = (currentMessage.length + maxMessageLength - 1) / maxMessageLength
      val nextPage = (state.currentMessagePage + 1) % totalPages
      state.copy(currentMessagePage = nextPage)
    } else {
      state
    }
  }

  @tailrec
  def loop(state: GameState): Unit = {
    renderGame(state)
    val input = inputReader.read().toChar.toLower
    val newState = handleInput(state, input)
    if (newState.running) loop(newState)
  }
  
  def renderGame(state: GameState): Unit = {
    // Clear screen
    print("\u001b[2J\u001b[H")
    
    // Render messages
    renderMessages(state)
    
    // Render map
    println(state.map.render(state.player, state.entities))
    println("Move with WASD, SPACE for messages, Q to quit:")
  }
  
  def renderMessages(state: GameState): Unit = {
    val maxMessageLength = state.map.width
    val currentMessages = state.messages
    
    if (currentMessages.nonEmpty) {
      val currentMessage = currentMessages.last
      val messageLength = currentMessage.length
      
      if (messageLength > maxMessageLength) {
        // Handle pagination
        val totalPages = (messageLength + maxMessageLength - 1) / maxMessageLength
        val startIdx = state.currentMessagePage * maxMessageLength
        val endIdx = math.min(startIdx + maxMessageLength, messageLength)
        val messagePart = currentMessage.substring(startIdx, endIdx)
        
        if (endIdx < messageLength) {
          // Show "more" indicator
          val paddedMessage = messagePart.padTo(maxMessageLength - 5, ' ')
          println(paddedMessage + "-more-")
        } else {
          println(messagePart)
        }
      } else {
        println(currentMessage.padTo(maxMessageLength, ' '))
      }
    } else {
      println(" ".padTo(maxMessageLength, ' '))
    }
  }

  def start(): Unit = {
    val map = createInitialMap()
    val player = Player(Position(1, 1))
    val entities = createInitialEntities()
    val initialState = GameState(map, player, entities, true, Vector("Welcome to Rogacy! Use WASD to move."), 0)
    loop(initialState)
  }
}
