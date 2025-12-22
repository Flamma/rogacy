package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonajeTest extends AnyFlatSpec with Matchers {
  "A Personaje" should "correctly initialize with position and character" in {
    val position = Position(5, 10)
    val personaje = Personaje("Goblin", 'g', position)
    
    personaje.position shouldEqual position
    personaje.symbol shouldEqual 'g'
    personaje.name shouldEqual "Goblin"
  }

  it should "be immutable" in {
    val personaje = Personaje("Goblin", 'g', Position(3, 7))
    // This would cause compilation error if Personaje was mutable
    // personaje.symbol = 'm' // This line would not compile
  }

  it should "generate correct interaction message" in {
    val goblin = Personaje("Goblin", 'g', Position(1, 1))
    val potion = Personaje("Potion", 'p', Position(2, 2))
    val orc = Personaje("Orc", 'o', Position(3, 3))

    goblin.interact() shouldEqual "You bump into the goblin!"
    potion.interact() shouldEqual "You bump into the potion!"
    orc.interact() shouldEqual "You bump into the orc!"
  }
}