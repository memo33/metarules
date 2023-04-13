package metarules.meta

import org.scalatest.{WordSpec, Matchers}

class RuleGeneratorSpec extends WordSpec with Matchers {

  "RuleGenerator" should {
    "should support meta syntax and RUL2-compatibility syntax" in {
      import internal.DummyNetwork._, RotFlip._, Flags._, Implicits._, group.SymGroup._

      val resolve = Map[Tile, IdTile](
        (Dirtroad~NS, IdTile(0x57000000, R0F0)),
        (Dirtroad~EW, IdTile(0x57000000, R1F0)),
        (Mis~NS,      IdTile(0x57020000, R0F0)),
        (Mis~EW,      IdTile(0x57020000, R1F0)),
        (Mis~SN,      IdTile(0x57020000, R2F0)),
        (Mis~WE,      IdTile(0x57020000, R3F0)),
        (L1Rhw2~NS,   IdTile(0x57100000, R0F0)),
        (L1Rhw2~EW,   IdTile(0x57100000, R1F0)))

      val generator = new RuleGenerator {
        var context = RuleTransducer.Context(resolve)
        def start(): Unit = {
          Rules += Mis~WE    | (Dirtroad ~> Mis)~WE
          Rules += L1Rhw2~WE | (Dirtroad ~> L1Rhw2)~WE
          Rules += IdTile(0xFF000100, R0F0, Cyc1) | (Dirtroad ~> Mis)~WE
          Rules += (Dirtroad ~> Mis)~WE | IdTile(0xFF000200, R2F0, (Road~EW).symmetries)
          Rules += L1Rhw2~WE | IdTile(0xFF000300, R0F0, (Road~EW).symmetries) | % | IdTile(0xFF000400, R0F0, (Road~EW).symmetries)
          Rules += IdTile(0xFF000500, R1F0, (Road~EW).symmetries) | L1Rhw2~WE | IdTile(0xFF000600, R1F0, (Road~EW).symmetries) | %
          createRules()
        }
      }

      generator.start()

      generator.queue shouldBe Seq(
        Rule(0x57020000,3,0,0x57000000,1,0,0x57020000,3,0,0x57020000,3,0),
        Rule(0x57020000,3,0,0x57000000,3,0,0x57020000,3,0,0x57020000,3,0),
        Rule(0x57020000,1,0,0x57000000,3,0,0x57020000,1,0,0x57020000,1,0),
        Rule(0x57020000,1,0,0x57000000,1,0,0x57020000,1,0,0x57020000,1,0),
        Rule(0x57100000,1,0,0x57000000,1,0,0x57100000,1,0,0x57100000,1,0),
        Rule(0x57100000,3,0,0x57000000,3,0,0x57100000,3,0,0x57100000,3,0),
        Rule(0xFF000100,0,0,0x57000000,1,0,0xFF000100,0,0,0x57020000,3,0),
        Rule(0xFF000100,0,0,0x57000000,3,0,0xFF000100,0,0,0x57020000,3,0),
        Rule(0xFF000100,2,1,0x57000000,3,0,0xFF000100,2,1,0x57020000,1,0),
        Rule(0xFF000100,2,1,0x57000000,1,0,0xFF000100,2,1,0x57020000,1,0),
        Rule(0x57000000,1,0,0xFF000200,2,0,0x57020000,3,0,0xFF000200,2,0),
        Rule(0x57000000,1,0,0xFF000200,0,0,0x57020000,3,0,0xFF000200,0,0),
        Rule(0x57000000,3,0,0xFF000200,2,0,0x57020000,3,0,0xFF000200,2,0),
        Rule(0x57000000,3,0,0xFF000200,0,0,0x57020000,3,0,0xFF000200,0,0),
        Rule(0x57100000,1,0,0xFF000300,0,0,0x57100000,1,0,0xFF000400,0,0),
        Rule(0x57100000,1,0,0xFF000300,2,0,0x57100000,1,0,0xFF000400,2,0),
        Rule(0x57100000,3,0,0xFF000300,0,0,0x57100000,3,0,0xFF000400,0,0),
        Rule(0x57100000,3,0,0xFF000300,2,0,0x57100000,3,0,0xFF000400,2,0),
        Rule(0xFF000500,1,0,0x57100000,1,0,0xFF000600,1,0,0x57100000,1,0),
        Rule(0xFF000500,3,0,0x57100000,3,0,0xFF000600,3,0,0x57100000,3,0))
    }
  }
}
