package net.asqueados.rogacy

import scala.annotation.tailrec
import org.jline.terminal.TerminalBuilder

object Game {
  private val terminal = TerminalBuilder.builder().system(true).build()
  terminal.enterRawMode()
  private val inputReader = terminal.reader()
  
  private val colorsSupported = {
    val maxColors = terminal.getNumericCapability(org.jline.utils.InfoCmp.Capability.max_colors)
    maxColors != null && (maxColors.asInstanceOf[Int] >= 8)
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
        // Player bumped into an entity, attack it
        val damagedEntity = entity.copy(hp = entity.hp - 1)
        val isDead = damagedEntity.hp <= 0
        val message = if (isDead) s"You kill the ${entity.coloredName}!" 
                      else s"You hit the ${entity.coloredName} (${damagedEntity.hp} hp left)."
        
        val newEntities = if (isDead) state.entities.filterNot(_ == entity)
                          else state.entities.map(e => if (e == entity) damagedEntity else e)
        
        state.copy(entities = newEntities, time = state.time + 100).addMessage(message)
      case None =>
        val tile = state.map.getTile(newPos.x, newPos.y)
        if (tile == '+') {
          val newMap = state.map.updateTile(newPos.x, newPos.y, '\'')
          state.copy(map = newMap, time = state.time + 100).addMessage("You open the door.")
        } else if (state.map.isWalkable(newPos.x, newPos.y, state.entities)) {
          state.copy(player = state.player.copy(position = newPos), time = state.time + 100)
        } else {
          state
        }
    }
  }

  def handleInput(state: GameState, input: Char): GameState = input match {
    case 'w' | '8' => movePlayer(state, 0, -1)
    case 's' | '2' => movePlayer(state, 0, 1)
    case 'a' | '4' => movePlayer(state, -1, 0)
    case 'd' | '6' => movePlayer(state, 1, 0)
    case '7' => movePlayer(state, -1, -1)
    case '9' => movePlayer(state, 1, -1)
    case '1' => movePlayer(state, -1, 1)
    case '3' => movePlayer(state, 1, 1)
    case '5' => state.copy(time = state.time + 100) // Wait/Pass turn
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
      state.copy(map = map, player = state.player.copy(position = downPos), entities = entities, depth = newDepth, time = state.time + 100)
        .addMessage(s"You ascend to level $newDepth.")
    } else if (!isUp && tile == '>') {
      val newDepth = state.depth + 1
      val (map, upPos, downPos, entities) = DungeonGenerator.generate(state.map.width, state.map.height)
      state.copy(map = map, player = state.player.copy(position = upPos), entities = entities, depth = newDepth, time = state.time + 100)
        .addMessage(s"You descend to level $newDepth.")
    } else {
      state.addMessage(if (isUp) "You can't go up here." else "You can't go down here.")
    }
  }
  
  def handleMessagePagination(state: GameState): GameState = {
    val maxMessageLength = Constants.ViewportWidth
    val currentMessages = state.messages
    if (currentMessages.isEmpty) return state
    
    val currentMessage = currentMessages(state.currentMessageIndex)
    val totalPages = (currentMessage.length + maxMessageLength - 1) / maxMessageLength
    
    if (state.currentMessagePage < totalPages - 1) {
      state.copy(currentMessagePage = state.currentMessagePage + 1)
    } else if (state.currentMessageIndex < currentMessages.size - 1) {
      state.copy(currentMessageIndex = state.currentMessageIndex + 1, currentMessagePage = 0)
    } else {
      state
    }
  }

  @tailrec
  def loop(state: GameState): Unit = {
    val updatedVisibilityState = state.updateVisibility()
    renderGame(updatedVisibilityState)
    
    // Check if we have pending messages to clear
    val hasMoreMessages = state.currentMessageIndex < state.messages.size - 1
    val currentMessage = if (state.messages.nonEmpty) state.messages(state.currentMessageIndex) else ""
    val totalPages = (currentMessage.length + Constants.ViewportWidth - 1) / Constants.ViewportWidth
    val hasMorePages = state.currentMessagePage < totalPages - 1
    
    if (hasMoreMessages || hasMorePages) {
      val input = inputReader.read().toChar.toLower
      if (input == ' ') {
        loop(handleMessagePagination(state))
      } else {
        // Only allow space to clear messages
        loop(state)
      }
    } else if (state.running) {
      val input = inputReader.read().toChar.toLower
      
      // Clear messages before processing new input if it's an action that takes time
      val stateWithClearedMessages = if (Set('w', 's', 'a', 'd', '1', '2', '3', '4', '6', '7', '8', '9', '5', '<', '>').contains(input)) {
        state.copy(messages = Vector.empty, currentMessageIndex = 0, currentMessagePage = 0)
      } else {
        state
      }

      val stateAfterInput = handleInput(stateWithClearedMessages, input)
      
      if (stateAfterInput.running && stateAfterInput.player.health > 0) {
        if (stateAfterInput.time > updatedVisibilityState.time) {
          // Player took an action, now process monsters
          val stateAfterMonsters = processMonsters(stateAfterInput)
          if (stateAfterMonsters.player.health <= 0) {
            val deadState = stateAfterMonsters.addMessage("You have died... Game Over.")
            loop(deadState)
          } else {
            loop(stateAfterMonsters)
          }
        } else {
          // No time passed (e.g. invalid input or message pagination)
          loop(stateAfterInput)
        }
      } else if (stateAfterInput.running && stateAfterInput.player.health <= 0) {
        // Player is dead, one last input to exit
        val deadState = stateAfterInput.copy(running = false)
        loop(deadState)
      }
    }
  }

  def processMonsters(state: GameState): GameState = {
    @tailrec
    def processAll(currentState: GameState): GameState = {
      // Find a monster that can act
      val readyMonster = currentState.entities.find(_.nextActionTime <= currentState.time)
      
      readyMonster match {
        case Some(monster) =>
          val newState = monsterAI(currentState, monster)
          processAll(newState)
        case None =>
          currentState
      }
    }
    processAll(state)
  }

  def monsterAI(state: GameState, monster: Personaje): GameState = {
    // Can monster see player?
    val canSeePlayer = state.map.hasLineOfSight(monster.position.x, monster.position.y, state.player.position.x, state.player.position.y)
    
    if (canSeePlayer) {
      // Find shortest path to player
      val nextPos = findNextStep(state, monster.position, state.player.position)
      
      if (nextPos == state.player.position) {
        // Attack player
        val newPlayer = state.player.copy(health = state.player.health - 1)
        val updatedMonster = monster.copy(nextActionTime = monster.nextActionTime + monster.speed)
        val newEntities = state.entities.map(e => if (e == monster) updatedMonster else e)
        state.copy(player = newPlayer, entities = newEntities).addMessage(s"The ${monster.coloredName} hits you!")
      } else if (state.map.isWalkable(nextPos.x, nextPos.y, state.entities)) {
        // Move monster
        val updatedMonster = monster.copy(position = nextPos, nextActionTime = monster.nextActionTime + monster.speed)
        val newEntities = state.entities.map(e => if (e == monster) updatedMonster else e)
        state.copy(entities = newEntities)
      } else {
        // Cannot move (blocked by other monster), just consume time
        val updatedMonster = monster.copy(nextActionTime = monster.nextActionTime + monster.speed)
        val newEntities = state.entities.map(e => if (e == monster) updatedMonster else e)
        state.copy(entities = newEntities)
      }
    } else {
      // Stay still, but must consume time to avoid infinite loop
      val updatedMonster = monster.copy(nextActionTime = monster.nextActionTime + monster.speed)
      val newEntities = state.entities.map(e => if (e == monster) updatedMonster else e)
      state.copy(entities = newEntities)
    }
  }

  def findNextStep(state: GameState, start: Position, target: Position): Position = {
    // Simple BFS for shortest path
    val queue = scala.collection.mutable.Queue(start)
    val cameFrom = scala.collection.mutable.Map[Position, Position]()
    cameFrom(start) = start
    
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current == target) {
        // Reconstruct path
        var step = target
        while (cameFrom(step) != start) {
          step = cameFrom(step)
        }
        return step
      }
      
      val neighbors = for {
        dx <- -1 to 1
        dy <- -1 to 1
        if dx != 0 || dy != 0
        next = Position(current.x + dx, current.y + dy)
        // A monster can "walk" onto the player's square to attack
        if state.map.isWalkable(next.x, next.y, state.entities) || next == target
      } yield next
      
      for (next <- neighbors if !cameFrom.contains(next)) {
        cameFrom(next) = current
        queue.enqueue(next)
      }
    }
    start // No path found
  }

  def renderGame(state: GameState): Unit = {
    // Clear screen
    print("\u001b[2J\u001b[H")
    
    // Render messages
    renderMessages(state)
    
    // Render map
    println(state.map.render(state.player, state.entities, colorsSupported, Constants.ViewportWidth, Constants.ViewportHeight, state.viewEverything))
    println(s"Level: ${state.depth} | Time: ${state.time} | HP: ${state.player.health} | WASD/Numpad: Move, < / >: Stairs, SPACE: Msg, Q: Quit")
  }
  
  def renderMessages(state: GameState): Unit = {
    val maxMessageLength = Constants.ViewportWidth
    val currentMessages = state.messages
    
    if (currentMessages.nonEmpty && state.currentMessageIndex < currentMessages.size) {
      val currentMessage = currentMessages(state.currentMessageIndex)
      val messageLength = currentMessage.length
      
      val totalPages = (messageLength + maxMessageLength - 1) / maxMessageLength
      val startIdx = state.currentMessagePage * maxMessageLength
      val endIdx = math.min(startIdx + maxMessageLength, messageLength)
      val messagePart = currentMessage.substring(startIdx, endIdx)
      
      val hasMorePages = state.currentMessagePage < totalPages - 1
      val hasMoreMessages = state.currentMessageIndex < currentMessages.size - 1
      
      if (hasMorePages || hasMoreMessages) {
        // Show "more" indicator
        val paddedMessage = messagePart.padTo(maxMessageLength - 6, ' ')
        print(paddedMessage + "-more-")
      } else {
        print(messagePart.padTo(maxMessageLength, ' '))
      }
    } else {
      print(" ".padTo(maxMessageLength, ' '))
    }
    println()
  }

  def start(viewEverything: Boolean): Unit = {
    val (map, upPos, downPos, entities) = DungeonGenerator.generate(200, 200)
    // Start at up stairs on first level
    val initialState = GameState(
      map, 
      Player(upPos), 
      entities, 
      true, 
      Vector("Welcome to Rogacy! Use WASD to move."), 
      0,
      0,
      1,
      viewEverything,
      0L
    ).updateVisibility()
    loop(initialState)
  }
}
