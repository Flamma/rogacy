package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EntityTest extends AnyFlatSpec with Matchers {
  "An Entity" should "correctly initialize with position and character" in {
    val position = Position(5, 10)
    val entity = Entity(position, 'g')
    
    entity.position shouldEqual position
    entity.char shouldEqual 'g'
  }

  it should "be immutable" in {
    val entity = Entity(Position(3, 7), 'g')
    // This would cause compilation error if Entity was mutable
    // entity.char = 'm' // This line would not compile
  }
}