package module.flexfly

import meta._, module._, Network._, RotFlip._, Flags._

object FlexFlyTiles {

  val T0 = (+2,0,-213,0)
  val T1 = (+213,0,-223,0)
  val T2 = (+223,0,0,0)
  //val unused = (0,0,-233,0)
  val T3 = (+233,0,-243,0)
  val T4 = (+243,0,0,-241)
  // T5 is T3 transposed
  val T6 = (+243,0,0,-1) // 45 degree curve diagonal end tile
  // T7 is T6 transposed

}

class FlexFlyResolver extends RhwResolver {

  private[this] val flexFlags = Set(Flag.In, Flag.Out) flatMap { x => Seq(
    x.FlexFly01L, x.FlexFly01R, x.FlexFly12L, x.FlexFly12R,
    x.FlexFly34L, x.FlexFly34R, x.FlexFly45L, x.FlexFly45R)
  }
  private def hasFlexFlyFlag(t: Tile) = t.segs.exists(_.flags exists flexFlags.contains)
  override def isDefinedAt(t: Tile) = hasFlexFlyFlag(t) || super.isDefinedAt(t)

  override def apply(tile: Tile): IdTile = if (!hasFlexFlyFlag(tile)) {
    super.apply(tile)
  } else {
    ???
  }

}
