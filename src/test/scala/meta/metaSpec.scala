package metarules.meta

import org.scalatest.{WordSpec, Matchers}

class metaSpec extends WordSpec with Matchers {

  "Rule use cases" should {
    "be compilable" in {
      import Flags._, Network._, Implicits._
      def % = Tile.CopyTile
      type TR = (Rule[Tile], Rule[Tile]) // tuple rule
      (Road~WE | Road~WE | % | Road~WE)  // : Rule[Tile]
      (Road~WE~EW | Road~WE | % | Road~WE)  // : TR
      (Road~WE~EW & Rail~NS | Road~WE | % | Road~WE)  // : TR
      (Road~WE & Rail~NS~SN | Road~WE | % | Road~WE)  // : TR
      (Road~WE~EW & Rail~NS~SN | Road~WE | % | Road~WE)  // : TR
      (Road~WE | Road~WE~EW | % | Road~WE~EW)  // : TR
      (Road~WE | Road~WE~EW & Rail~NS | % | Road~WE~EW)  // : TR
      (Road~WE | Road~WE & Rail~NS~SN | % | Road~WE~EW)  // : TR
      (Road~WE | Road~WE~EW & Rail~NS~SN | % | Road~WE~EW)  // : TR
      (Road~WE & Rail~NS~SN | Road~WE~EW & Rail~NS~SN | % | Road~WE~EW)  // : TR
      (Road~WE~EW & Rail~NS | Road~WE~EW & Rail~NS~SN | % | Road~WE~EW)  // : TR

      (Road~WE | (Road ~> Street)~WE)  // : Rule[Tile]
      (Road~WE | (Road ~> Street)~WE & Rail~NS)  // : Rule[Tile]
      (Road~WE | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS)  // : Rule[Tile]
      (Road~WE | Road~WE & (Rail ~> Lightrail)~NS)  // : Rule[Tile]
      (Road~WE & Rail~NS | (Road ~> Street)~WE)  // : Rule[Tile]
      (Road~WE & Rail~NS | (Road ~> Street)~WE & Rail~NS)  // : Rule[Tile]
      (Road~WE & Rail~NS | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS)  // : Rule[Tile]
      (Road~WE & Rail~NS | Road~WE & (Rail ~> Lightrail)~NS)  // : Rule[Tile]
      ((Road ~> Street)~WE | Road~WE)  // : Rule[Tile]
      ((Road ~> Street)~WE & Rail~NS | Road~WE)  // : Rule[Tile]
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS | Road~WE)  // : Rule[Tile]
      (Road~WE & (Rail ~> Lightrail)~NS | Road~WE)  // : Rule[Tile]
      ((Road ~> Street)~WE | (Road ~> Street)~WE)  // : Rule[Tile]
      ((Road ~> Street)~WE & Rail~NS | (Road ~> Street)~WE)  // : Rule[Tile]
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS | (Road ~> Street)~WE)  // : Rule[Tile]
      (Road~WE & (Rail ~> Lightrail)~NS | (Road ~> Street)~WE)  // : Rule[Tile]

      (Road~WE | (Road ~> Street)~WE~EW)  // : TR
      (Road~WE | (Road ~> Street)~WE & Rail~NS~SN)  // : TR
      (Road~WE | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS~SN)  // : TR
      (Road~WE | Road~WE~EW & (Rail ~> Lightrail)~NS)  // : TR
      (Road~WE & Rail~NS~SN | (Road ~> Street)~WE)  // : TR
      (Road~WE~EW & Rail~NS | (Road ~> Street)~WE & Rail~NS)  // : TR
      (Road~WE & Rail~NS | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS~SN)  // : TR
      (Road~WE & Rail~NS | Road~WE~EW & (Rail ~> Lightrail)~NS)  // : TR
      ((Road ~> Street)~WE | Road~WE~EW & Rail~NS)  // : TR
      ((Road ~> Street)~WE & Rail~NS~SN | Road~WE)  // : TR
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS | Road~WE~EW)  // : TR
      (Road~WE~EW & (Rail ~> Lightrail)~NS | Road~WE)  // : TR
      ((Road ~> Street)~WE~EW | (Road ~> Street)~WE)  // : TR
      ((Road ~> Street)~WE & Rail~NS | (Road ~> Street)~WE~EW)  // : TR
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS~SN | (Road ~> Street)~WE)  // : TR
      (Road~WE~EW & (Rail ~> Lightrail)~NS | (Road ~> Street)~WE)  // : TR
    }
  }

  "Metarule syntax" should {
    "support compatibility mode with RUL2 syntax" in {
      import Flags._, Network._, Implicits._
      import RotFlip._, group.SymGroup.Cyc1
      def % = Tile.CopyTile
      type TR = (Rule[SymTile], Rule[SymTile]) // tuple rule
      val it = IdTile(0x12345678, R0F0, Cyc1)

      (it | Road~WE | it | Road~WE)  // : Rule[SymTile]
      (it | Road~WE | % | Road~WE)  // : Rule[SymTile]
      (it | Road~WE & Rail~NS | % | %)  // : Rule[SymTile]
      (it | Road~WE & Rail~NS | it | %)  // : Rule[SymTile]
      (it | Road~WE & Rail~NS | % | it)  // : Rule[SymTile]
      (it | Road~WE & Rail~NS | it | it)  // : Rule[SymTile]
      (it | Road~WE & Rail~NS & Lightrail~ES | % | %)  // : Rule[SymTile]
      (it | Road~WE~EW | it | Road~WE~EW)  // : TR
      (it | Road~WE~EW & Rail~NS | % | Road~WE~EW & Rail~NS)  // : TR
      (it | Road~WE~EW & Rail~NS | it | Road~WE~EW & Rail~NS)  // : TR
      (it | (Road ~> Road)~WE)  // : Rule[SymTile]
      (it | (Road ~> Road)~WE & Rail~NS)  // : Rule[SymTile]
      (it | (Road ~> Road)~WE & Rail~NS~SN)  // : TR
      (it | (Road ~> Road)~WE~EW & Rail~NS)  // : TR

      (Road~WE | it | Road~WE | it)  // : Rule[SymTile]
      (Road~WE | it | Road~WE | %)  // : Rule[SymTile]
      (Road~WE & Rail~NS | it | % | %)  // : Rule[SymTile]
      (Road~WE & Rail~NS | it | % | it)  // : Rule[SymTile]
      (Road~WE & Rail~NS | it | it | %)  // : Rule[SymTile]
      (Road~WE & Rail~NS | it | it | it)  // : Rule[SymTile]
      (Road~WE & Rail~NS & Lightrail~ES | it | % | %)  // : Rule[SymTile]
      (Road~WE~EW | it | Road~WE~EW | it)  // : TR
      (Road~WE~EW & Rail~NS | it | Road~WE~EW & Rail~NS | %)  // : TR
      (Road~WE~EW & Rail~NS | it | Road~WE~EW & Rail~NS | it)  // : TR
      ((Road ~> Road)~WE | it)  // : Rule[SymTile]
      ((Road ~> Road)~WE & Rail~NS | it)  // : Rule[SymTile]
      ((Road ~> Road)~WE & Rail~NS~SN | it)  // : TR
      ((Road ~> Road)~WE~EW & Rail~NS | it)  // : TR

      (it | it | it | it)
      (it | it | % | it)
      (it | it | it | %)
      (it | it | it | Road~EW)
      (it | it | Road~EW | it)
      (it | it | Road~EW | Road~EW)
      (it | Road~EW | it | it)
      (Road~EW~WE | it | it | it)
      (Road~EW | Road~EW & Rail~NS | it | it)
      (Road~EW | Road~EW | it | it)
      (Road~EW | Road~EW | it | Road~EW)
      (Road~EW | Road~EW | Road~EW | it)
    }
  }

  "Flag" should {
    import Flag._
    "flip correctly" in {
      Bi.flip(2) shouldBe 2
      InOut.flip(2) shouldBe -2
      InOut.flip(-2) shouldBe 2
      Bi.flip(1) shouldBe 3
      Bi.flip(3) shouldBe 1
      InOut.flip(1) shouldBe -3
      InOut.flip(3) shouldBe -1
      InOut.flip(-1) shouldBe 3
      InOut.flip(-3) shouldBe 1
    }
    "reverse correctly" in {
      Bi.reverse(2) shouldBe 2
      InOut.reverse(2) shouldBe -2
      InOut.reverse(-2) shouldBe (2)
      InOut.reverse(1) shouldBe -1
      InOut.reverse(3) shouldBe -3
      InOut.reverse(-1) shouldBe 1
      InOut.reverse(-3) shouldBe 3
    }
    "display correctly" in {
      Flags(0,1,2,-3,Bi).toString shouldBe "(0,1,2,3)"
      Flags(0,1,2,-3,InOut).toString shouldBe "(0,+1,+2,-3)"
      Flags(0,1,2,-3,LeftSpinBi).toString shouldBe "(0,1L,2L,3L)"
      Flags(0,1,2,-3,LeftSpinInOut).toString shouldBe "(0,+1L,+2L,-3L)"
    }
    "not compare equal for different manifests" in {
      Flags(0,2,0,-2,LeftSpinBi) should not be Flags(0,2,0,-2,RightSpinBi)
      Flags(0,2,0,-2,LeftSpinBi) shouldBe Flags(0,2,0,-2,LeftSpinBi)
    }
  }

  "Segment" should {
    import Flags._, Network._, Implicits._
    "reverse correctly" in {
      for (n <- Network.values) {
        (n~NS).reverse should be (n~SN)
        (n~WE).reverse should be (n~EW)
        (n~ES).reverse should be (n~SE)
        (n~NC).reverse should be (n~CN)
      }
    }
    "rotate and flip correctly" in {
      import RotFlip._
      (Avenue~NS) * R1F0 shouldBe Avenue~EW
      (Avenue~NS) * R3F0 shouldBe Avenue~WE
      (Avenue~NS) * R0F1 shouldBe Avenue~SN
      (Avenue~NS) * R1F1 shouldBe Avenue~EW
    }
    "reverse shared-tile diagonals" ignore {
      // does not currently work, or does it? depends on what you expect
      for (n <- Network.values) {
        (n~SharedDiagRight).reverse should be (n~SharedDiagRight)
      }
    }
  }

  "IdSymTile" should {
    "respect symmerties upon rotation" in {
      import RotFlip._, group.SymGroup._
      val it = IdTile(0x12345678, R1F0, Cyc2B)
      (it * R1F0).symmetries shouldBe Cyc2D
      (it * R2F0).symmetries shouldBe Cyc2B
      (it * R1F0).rf shouldBe R2F0
    }
  }
}
