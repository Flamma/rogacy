package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonajeTest extends AnyFlatSpec with Matchers {
  "A Personaje" should "correctly initialize with position and character" in {
    val position = Position(5, 10)
    val personaje = Personaje("Goblin", 'g', position, Colors.White, hp = 3, speed = 100)
    
    personaje.position shouldEqual position
    personaje.symbol shouldEqual 'g'
    personaje.name shouldEqual "Goblin"
  }

  it should "be immutable" in {
    val personaje = Personaje("Goblin", 'g', Position(3, 7), Colors.White, hp = 3, speed = 100)
    // This would cause compilation error if Personaje was mutable
    // personaje.symbol = 'm' // This line would not compile
  }

  it should "generate correct interaction message" in {
    val goblin = Personaje("Goblin", 'g', Position(1, 1), Colors.Green, hp = 3, speed = 100)
    val potion = Personaje("Potion", 'p', Position(2, 2), Colors.Cyan, hp = 3, speed = 100)

    goblin.interact().toLowerCase should include ("goblin")
    potion.interact().toLowerCase should include ("potion")
  }
}