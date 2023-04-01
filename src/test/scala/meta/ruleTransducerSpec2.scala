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
    map.toMap
  }

  "RuleTransducer" should {
    "resolve ambiguities in the best non-optimal way (it's a bandaid)" in {
      RuleTransducer(L2Rhw2~EW & Rhw4~NS | (Dirtroad~>L2Rhw2)~EW & L1Rhw2~NS)(resolver).toSeq shouldBe Seq(
        // fails in world map NS direction
        //   Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,0,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,3,0)),
        //   Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,2,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,1,0))
        // fails in world map SN direction
        //   Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,0,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,3,0)),
        //   Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,2,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,1,0))
        // fails both in NS and SN direction
        //   Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,0,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,3,0)),
        //   Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,2,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,1,0))
        // This one mostly works with just a tiny instability in EW direction (apart
        // from the instability, this works not because the orientations are always
        // correct, but in the case they are wrong, the game evaluates the overrides in
        // a suitable order in the world map direction West to East).
        Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,0,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,3,0)),
        Rule(IdTile(0x57201D00,3,0), IdTile(0x57101A00,2,0), IdTile(0x57201D00,3,0), IdTile(0x57201A10,1,0))
      )
    }
  }
}

