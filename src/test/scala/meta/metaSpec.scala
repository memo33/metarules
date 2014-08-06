package meta

import org.scalatest.{WordSpec, Matchers}

class metaSpec extends WordSpec with Matchers {

  "Rule use cases" should {
    "be compilable" in {
      import Flags._, Network._, Implicits._
      def % = Tile.CopyTile
      type TR = (Rule[Tile], Rule[Tile]) // tuple rule
      (Road~WE | Road~WE | % | Road~WE): Rule[Tile]
      (Road~WE~EW | Road~WE | % | Road~WE): TR
      (Road~WE~EW & Rail~NS | Road~WE | % | Road~WE): TR
      (Road~WE & Rail~NS~SN | Road~WE | % | Road~WE): TR
      (Road~WE~EW & Rail~NS~SN | Road~WE | % | Road~WE): TR
      (Road~WE | Road~WE~EW | % | Road~WE~EW): TR
      (Road~WE | Road~WE~EW & Rail~NS | % | Road~WE~EW): TR
      (Road~WE | Road~WE & Rail~NS~SN | % | Road~WE~EW): TR
      (Road~WE | Road~WE~EW & Rail~NS~SN | % | Road~WE~EW): TR
      (Road~WE & Rail~NS~SN | Road~WE~EW & Rail~NS~SN | % | Road~WE~EW): TR
      (Road~WE~EW & Rail~NS | Road~WE~EW & Rail~NS~SN | % | Road~WE~EW): TR

      (Road~WE | (Road ~> Street)~WE): Rule[Tile]
      (Road~WE | (Road ~> Street)~WE & Rail~NS): Rule[Tile]
      (Road~WE | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS): Rule[Tile]
      (Road~WE | Road~WE & (Rail ~> Lightrail)~NS): Rule[Tile]
      (Road~WE & Rail~NS | (Road ~> Street)~WE): Rule[Tile]
      (Road~WE & Rail~NS | (Road ~> Street)~WE & Rail~NS): Rule[Tile]
      (Road~WE & Rail~NS | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS): Rule[Tile]
      (Road~WE & Rail~NS | Road~WE & (Rail ~> Lightrail)~NS): Rule[Tile]
      ((Road ~> Street)~WE | Road~WE): Rule[Tile]
      ((Road ~> Street)~WE & Rail~NS | Road~WE): Rule[Tile]
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS | Road~WE): Rule[Tile]
      (Road~WE & (Rail ~> Lightrail)~NS | Road~WE): Rule[Tile]
      ((Road ~> Street)~WE | (Road ~> Street)~WE): Rule[Tile]
      ((Road ~> Street)~WE & Rail~NS | (Road ~> Street)~WE): Rule[Tile]
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS | (Road ~> Street)~WE): Rule[Tile]
      (Road~WE & (Rail ~> Lightrail)~NS | (Road ~> Street)~WE): Rule[Tile]

      (Road~WE | (Road ~> Street)~WE~EW): TR
      (Road~WE | (Road ~> Street)~WE & Rail~NS~SN): TR
      (Road~WE | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS~SN): TR
      (Road~WE | Road~WE~EW & (Rail ~> Lightrail)~NS): TR
      (Road~WE & Rail~NS~SN | (Road ~> Street)~WE): TR
      (Road~WE~EW & Rail~NS | (Road ~> Street)~WE & Rail~NS): TR
      (Road~WE & Rail~NS | (Road ~> Street)~WE & (Rail ~> Lightrail)~NS~SN): TR
      (Road~WE & Rail~NS | Road~WE~EW & (Rail ~> Lightrail)~NS): TR
      ((Road ~> Street)~WE | Road~WE~EW & Rail~NS): TR
      ((Road ~> Street)~WE & Rail~NS~SN | Road~WE): TR
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS | Road~WE~EW): TR
      (Road~WE~EW & (Rail ~> Lightrail)~NS | Road~WE): TR
      ((Road ~> Street)~WE~EW | (Road ~> Street)~WE): TR
      ((Road ~> Street)~WE & Rail~NS | (Road ~> Street)~WE~EW): TR
      ((Road ~> Street)~WE & (Rail ~> Lightrail)~NS~SN | (Road ~> Street)~WE): TR
      (Road~WE~EW & (Rail ~> Lightrail)~NS | (Road ~> Street)~WE): TR
    }
  }

  "Flag" should {
    import Flag._
    "initialize flip correctly" in {
      Bi.Orth.flip should be (Bi.Orth)
      In.Orth.flip should be (Out.Orth)
      Out.Orth.flip should be (In.Orth)
    }
    "initialize reverse correctly" in {
      Bi.Orth.reverse should be (Bi.Orth)
      In.Orth.reverse should be (Out.Orth)
      Out.Orth.reverse should be (In.Orth)
      In.DiagLeft.reverse should be (Out.DiagLeft)
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
    "reverse shared-tile diagonals" ignore {
      // does not currently work, or does it? depends on what you expect
      for (n <- Network.values) {
        (n~SharedDiagRight).reverse should be (n~SharedDiagRight)
      }
    }
  }
}
