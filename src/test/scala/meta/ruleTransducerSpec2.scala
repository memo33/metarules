package metarules.meta

import org.scalatest.{WordSpec, Matchers}
import Implicits._
import RotFlip._
import Network._
import Flags._

class RuleTransduceSpec2 extends WordSpec with Matchers {

  val resolver = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = IdTile(id, rf)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    add(L2Rhw2~NS & Rhw4~EW, 0x57201D00)
    add(Dirtroad~EW & L1Rhw2~NS, 0x57101A00)
    add(L2Rhw2~NS & L1Rhw2~EW, 0x57201A10)
    add(L2Rhw2~NS, 0x57200000)
    add(Dirtroad~NS, 0x57000000)
    add(Ave6m~ES & Ave6m~WS, 0x51219E8E)
    add(Ave6m~ES & Road~WS, 0x51218180)
    add(Road~ES & Road~SW, 0x00000700)
    map.toMap
  }

  "RuleTransducer" should {
    "detect ambiguities and take them into account in subsequent metarules" in {
      val context = RuleTransducer.Context(resolver, collection.mutable.Map.empty[Int, Set[RotFlip]])
      /* Initially, the possible absolute rotations of this tile are just these two.
       */
      (L2Rhw2~EW & L1Rhw2~NS).toIdSymTile(resolver).repr shouldBe (R0F0+R1F0)
      /* That is why the rule transducer only produces these two lines of RUL2.
       */
      RuleTransducer(L2Rhw2~EW & L1Rhw2~NS | (Dirtroad~>L2Rhw2)~EW)(context).toSeq shouldBe Seq(
        Rule(0x57201A10,1,0,0x57000000,1,0,0x57201A10,1,0,0x57200000,1,0),
        Rule(0x57201A10,3,0,0x57000000,3,0,0x57201A10,3,0,0x57200000,3,0))
      /* In the following configuration, the tile can end up with an absolute
       * rotation of R2F0. Since RUL2 code is based on relative rotations that
       * is automatically applied in various orientations of the world map, this
       * ambiguity cannot be avoided.
       */
      RuleTransducer(L2Rhw2~EW & Rhw4~NS | (Dirtroad~>L2Rhw2)~EW & L1Rhw2~NS)(context).toSeq shouldBe Seq(
        // (The following comments are outdated and not relevant anymore after the introduction of tileOrientationCache)
        //   fails in world map NS direction
        //     Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        //     Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0)
        //   fails in world map SN direction
        //     Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        //     Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0)
        //   fails both in NS and SN direction
        //     Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        //     Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0)
        //   This one mostly works with just a tiny instability in EW direction (apart
        //   from the instability, this works not because the orientations are always
        //   correct, but in the case they are wrong, the game evaluates the overrides in
        //   a suitable order in the world map direction West to East).
        Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0))
      /* Therefore, we store the information that the tile can have additional rotations.
       */
      context.tileOrientationCache shouldBe Map(0x57201A10 -> (R0F0+R1F0+R2F0+R3F0))
      /* And subsequently we take the additional rotations into account in all other metarules,
       * so the following metarule now produces four lines of RUL2 (instead of two).
       */
      RuleTransducer(L2Rhw2~EW & L1Rhw2~NS | (Dirtroad~>L2Rhw2)~EW)(context).toSeq shouldBe Seq(
        Rule(0x57201A10,1,0,0x57000000,1,0,0x57201A10,1,0,0x57200000,1,0),
        Rule(0x57201A10,1,0,0x57000000,3,0,0x57201A10,1,0,0x57200000,3,0),
        Rule(0x57201A10,3,0,0x57000000,1,0,0x57201A10,3,0,0x57200000,1,0),
        Rule(0x57201A10,3,0,0x57000000,3,0,0x57201A10,3,0,0x57200000,3,0))
      /* The second run should not change the cached orientations anymore.
       */
      context.tileOrientationCache shouldBe Map(0x57201A10 -> (R0F0+R1F0+R2F0+R3F0))
      RuleTransducer(L2Rhw2~EW & Rhw4~NS | (Dirtroad~>L2Rhw2)~EW & L1Rhw2~NS)(context).toSeq shouldBe Seq(
        Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0))
      context.tileOrientationCache shouldBe Map(0x57201A10 -> (R0F0+R1F0+R2F0+R3F0))
    }

    "detect ambiguities of tiles involving mirrorings" in {
      val context = RuleTransducer.Context(resolver, collection.mutable.Map.empty[Int, Set[RotFlip]])
      (Ave6m~ES & Ave6m~NE).toIdSymTile(resolver).repr shouldBe (R0F0+R1F0+R2F0+R3F0)
      /* Originally, we just get one line of RUL2.
       */
      RuleTransducer(Ave6m~ES & Ave6m~NE | (Road~>Ave6m)~NW & (Road~>Ave6m)~WS)(context).toSeq shouldBe Seq(
        Rule(0x51219E8E,3,0,0x00000700,1,0,0x51219E8E,3,0,0x51219E8E,1,0))
      (Ave6m~ES & Ave6m~NE).toIdSymTile(resolver).repr shouldBe (R0F0+R1F0+R2F0+R3F0)
      /* But here we notice that the tile can end up flipped.
       */
      RuleTransducer(Ave6m~ES & (Road~>Ave6m)~NE | (Road~>Ave6m)~NW & Ave6m~WS)(context).toSeq shouldBe Seq(
        Rule(0x51218180,3,0,0x51218180,3,1,0x51219E8E,1,1,0x51219E8E,3,1))
      context.tileOrientationCache shouldBe Map(0x51219E8E -> (R0F0+R1F0+R2F0+R3F0+R0F1+R3F1+R2F1+R1F1))
      /* Hence subsequently the possibility of flipped absolute rotations
       * results in more lines of RUL2.
       */
      RuleTransducer(Ave6m~ES & Ave6m~NE | (Road~>Ave6m)~NW & (Road~>Ave6m)~WS)(context).toSeq shouldBe Seq(
        Rule(0x51219E8E,3,0,0x00000700,1,0,0x51219E8E,3,0,0x51219E8E,1,0),
        Rule(0x51219E8E,3,0,0x00000700,3,1,0x51219E8E,3,0,0x51219E8E,3,1))
    }
  }
}

