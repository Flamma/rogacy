package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DungeonGeneratorTest extends AnyFlatSpec with Matchers {
  "DungeonGenerator" should "generate a map with correct dimensions" in {
    val width = 80
    val height = 20
    val (map, _, _, _) = DungeonGenerator.generate(width, height)
    
    map.width shouldBe width
    map.height shouldBe height
  }

  it should "place stairs reasonably far apart" in {
    val width = 80
    val height = 20
    val (_, upPos, downPos, _) = DungeonGenerator.generate(width, height)
    
    val dx = scala.math.abs(upPos.x - downPos.x)
    dx should be >= (width / 2)
  }

  it should "place stairs at correct tiles" in {
    val width = 80
    val height = 20
    val (map, upPos, downPos, _) = DungeonGenerator.generate(width, height)
    
    map.getTile(upPos.x, upPos.y) shouldBe '<'
    map.getTile(downPos.x, downPos.y) shouldBe '>'
  }

  it should "not place entities on stairs" in {
    val width = 80
    val height = 20
    val (map, upPos, downPos, entities) = DungeonGenerator.generate(width, height)
    
    entities.foreach { entity =>
      entity.position shouldNot be(upPos)
      entity.position shouldNot be(downPos)
      map.getTile(entity.position.x, entity.position.y) shouldBe '.'
    }
  }

  it should "generate walkable stairs" in {
    val width = 80
    val height = 20
    val (map, upPos, downPos, _) = DungeonGenerator.generate(width, height)
    
    map.isWalkable(upPos.x, upPos.y) shouldBe true
    map.isWalkable(downPos.x, downPos.y) shouldBe true
  }
}
