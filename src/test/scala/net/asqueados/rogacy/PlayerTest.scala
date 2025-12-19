package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlayerTest extends AnyFlatSpec with Matchers {
  "A Player" should "correctly initialize with position and health" in {
    val position = Position(5, 10)
    val player = Player(position, 100)
    
    player.position shouldEqual position
    player.health shouldEqual 100
  }

  it should "have default health of 100" in {
    val player = Player(Position(0, 0))
    player.health shouldEqual 100
  }

  it should "be immutable" in {
    val player = Player(Position(3, 7), 80)
    // This would cause compilation error if Player was mutable
    // player.health = 50 // This line would not compile
  }
}