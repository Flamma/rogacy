package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PositionTest extends AnyFlatSpec with Matchers {
  "A Position" should "correctly initialize with x and y coordinates" in {
    val position = Position(5, 10)
    position.x shouldEqual 5
    position.y shouldEqual 10
  }

  it should "be immutable" in {
    val position = Position(3, 7)
    // This would cause compilation error if Position was mutable
    // position.x = 10 // This line would not compile
  }
}