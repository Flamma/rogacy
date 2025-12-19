package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SillyTest extends AnyFlatSpec with Matchers {

  "A silly calculator" should "add numbers correctly" in {
    1 + 1 shouldEqual 2
  }

  it should "know that cats are superior to dogs" in {
    val cats = "meow"
    val dogs = "woof"
    cats.length should be <= dogs.length
  }

  "A banana" should "be yellow" in {
    val banana = "🍌"
    banana should not be empty
  }

  "The answer to life" should "be 42" in {
    21 * 2 shouldEqual 42
  }

  "A silly string" should "contain silly things" in {
    val silly = "silly willy nilly"
    silly should include("silly")
    silly.count(_.toString == "l") should be > 3
  }
}