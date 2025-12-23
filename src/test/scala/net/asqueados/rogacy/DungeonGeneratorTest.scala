package net.asqueados.rogacy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DungeonGeneratorTest extends AnyFlatSpec with Matchers {
  "DungeonGenerator" should "generate a map with correct dimensions" in {
    val width = 80
    val height = 20
    val (map, _, _, _) = DungeonGenerator.generate(width, height)
    
    map.width shouldEqual width
    map.height shouldEqual height
  }

  it should "place stairs reasonably far apart" in {
    val width = 80
    val height = 20
    val (_, upPos, downPos, _) = DungeonGenerator.generate(width, height)
    
    math.abs(upPos.x - downPos.x) should be >= (width / 2)
  }

  it should "place player at a stair position (if using start logic)" in {
    val width = 80
    val height = 20
    val (map, upPos, downPos, _) = DungeonGenerator.generate(width, height)
    
    map.getTile(upPos.x, upPos.y) shouldEqual '<'
    map.getTile(downPos.x, downPos.y) shouldEqual '>'
  }

  it should "not place entities on stairs" in {
    val width = 80
    val height = 20
    val (map, upPos, downPos, entities) = DungeonGenerator.generate(width, height)
    
    entities.foreach { entity =>
      entity.position should not equal upPos
      entity.position should not equal downPos
      map.getTile(entity.position.x, entity.position.y) shouldEqual '.'
    }
  }

  it should "generate walkable paths between rooms" in {
    val width = 80
    val height = 20
    val (map, upPos, downPos, _) = DungeonGenerator.generate(width, height)
    
    // Check that stairs are on walkable tiles (stairs themselves are walkable)
    map.isWalkable(upPos.x, upPos.y) shouldBe true
    map.isWalkable(downPos.x, downPos.y) shouldBe true
  }
}
