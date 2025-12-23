package net.asqueados.rogacy

import scala.annotation.tailrec
import org.jline.terminal.TerminalBuilder
import org.jline.reader.LineReaderBuilder

object Game {
  private val terminal = TerminalBuilder.builder().system(true).build()
  terminal.enterRawMode()
  private val inputReader = terminal.reader()
  
  private val colorsSupported = {
    val maxColors = terminal.getNumericCapability(org.jline.utils.InfoCmp.Capability.max_colors)
    maxColors != null && maxColors >= 8
  }

  def movePlayer(
    state: GameState,
    dx: Int, 
    dy: Int
  ): GameState = {
    val newPos = Position(state.player.position.x + dx, state.player.position.y + dy)
    
    // Check if there's an entity at the new position
    state.entities.find(_.position == newPos) match {
      case Some(entity) =>
        // Player bumped into an entity, use its interact method
        state.addMessage(entity.interact())
      case None =>
        val tile = state.map.getTile(newPos.x, newPos.y)
        if (tile == '+') {
          val newMap = state.map.updateTile(newPos.x, newPos.y, '\'')
          state.copy(map = newMap).addMessage("You open the door.")
        } else if (state.map.isWalkable(newPos.x, newPos.y, state.entities)) {
          val dirStr = if (dx > 0) "right" else if (dx < 0) "left" else if (dy > 0) "down" else "up"
          state.copy(player = state.player.copy(position = newPos)).addMessage(s"You moved $dirStr.")
        } else {
          state
        }
    }
  }

  def handleInput(state: GameState, input: Char): GameState = input match {
    case 'w' => movePlayer(state, 0, -1)
    case 's' => movePlayer(state, 0, 1)
    case 'a' => movePlayer(state, -1, 0)
    case 'd' => movePlayer(state, 1, 0)
    case '<' => handleStairs(state, isUp = true)
    case '>' => handleStairs(state, isUp = false)
    case ' ' => handleMessagePagination(state)
    case 'q' => state.copy(running = false)
    case _ => state
  }
  
  def handleStairs(state: GameState, isUp: Boolean): GameState = {
    val tile = state.map.getTile(state.player.position.x, state.player.position.y)
    if (isUp && tile == '<') {
      val newDepth = state.depth - 1
      val (map, upPos, downPos, entities) = DungeonGenerator.generate(state.map.width, state.map.height)
      state.copy(map = map, player = state.player.copy(position = downPos), entities = entities, depth = newDepth)
        .addMessage(s"You ascend to level $newDepth.")
    } else if (!isUp && tile == '>') {
      val newDepth = state.depth + 1
      val (map, upPos, downPos, entities) = DungeonGenerator.generate(state.map.width, state.map.height)
      state.copy(map = map, player = state.player.copy(position = upPos), entities = entities, depth = newDepth)
        .addMessage(s"You descend to level $newDepth.")
    } else {
      state.addMessage(if (isUp) "You can't go up here." else "You can't go down here.")
    }
  }

  def start(): Unit = {
    val (map, upPos, downPos, entities) = DungeonGenerator.generate(80, 20)
    // Start at up stairs on first level
    val initialState = GameState(map, Player(upPos), entities, true, Vector("Welcome to Rogacy! Use WASD to move."), 0)
    loop(initialState)
  }
}
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
    println(state.map.render(state.player, state.entities, colorsSupported))
    println(s"Level: ${state.depth} | WASD: Move, < / >: Stairs, SPACE: Msg, Q: Quit")
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
    val (map, player, entities) = DungeonGenerator.generate(80, 20)
    val initialState = GameState(map, player, entities, true, Vector("Welcome to Rogacy! Use WASD to move."), 0)
    loop(initialState)
  }
}
