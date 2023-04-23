package io.github.memo33
package metarules.meta

trait ImplicitsSyntax { syntax: Syntax =>

/** Defines a number of implicit conversions most of which are needed to make
  * the MetaRUL syntax work.
  */
object Implicits extends LowPriorityImplicits {
  import scala.language.implicitConversions
  implicit def segmentToTile(seg: Segment): Tile = Tile(Set(seg))
  implicit def tupleSegmentToTupleTile(ts: TupleSegment): TupleTile = TupleTile(ts.seg1, ts.seg2)
  implicit def tileToTupleTile(tile: Tile): TupleTile = TupleTile(tile, tile)
  implicit def coupleSegmentToCoupleTile(cs: CoupleSegment): CoupleTile = CoupleTile(cs.seg1, cs.seg2)
  implicit def tileToCoupleTile(tile: Tile): CoupleTile = CoupleTile(tile, tile)
  implicit def tupleCoupleSegmentToTupleCoupleTile(tcs: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(tcs.cseg1, tcs.cseg2)
  implicit def tileToTupleCoupleTile(tile: Tile): TupleCoupleTile = TupleCoupleTile(tile, tile)
  implicit def tupleTileToTupleCoupleTile(tt: TupleTile): TupleCoupleTile = TupleCoupleTile(tt.tile1, tt.tile2)
  implicit def coupleTileToTupleCoupleTile(ct: CoupleTile): TupleCoupleTile = TupleCoupleTile(ct, ct)
  implicit def segmentToTupleSegment(seg: Segment): TupleSegment = TupleSegment(seg, seg)
  implicit def symTileToTupleSymTile(tile: SymTile): TupleSymTile = TupleSymTile(tile, tile)
  implicit def tupleTileToTupleSymTile(tt: TupleTile): TupleSymTile = TupleSymTile(tt.tile1, tt.tile2)
  implicit def partialTileRule2ToSymTileRule(rule: Rule.PartialRule2[Tile, Rule[Tile]]): Rule.PartialRule2[SymTile, Rule[SymTile]] = {
    val res = rule | Rule.CopyTile | Rule.CopyTile
    new Rule.PartialRule2(Rule.newBuilder[SymTile] += res(0) += res(1))
  }
  implicit def partialTileRule3ToSymTileRule(rule: Rule.PartialRule3[Tile, Rule[Tile]]): Rule.PartialRule3[SymTile, Rule[SymTile]] = {
    val res = rule | Rule.CopyTile
    new Rule.PartialRule3(Rule.newBuilder[SymTile] += res(0) += res(1) += res(2))
  }
}

trait LowPriorityImplicits {
  import scala.language.implicitConversions
  implicit def segmentToTupleTile(seg: Segment): TupleTile = Implicits.tileToTupleTile(Implicits.segmentToTile(seg))
  implicit def segmentToCoupleTile(seg: Segment): CoupleTile = Implicits.tileToCoupleTile(Implicits.segmentToTile(seg))
  implicit def segmentToTupleCoupleTile(seg: Segment): TupleCoupleTile = Implicits.tileToTupleCoupleTile(Implicits.segmentToTile(seg))
  implicit def coupleSegmentToTupleCoupleTile(cs: CoupleSegment): TupleCoupleTile = Implicits.coupleTileToTupleCoupleTile(Implicits.coupleSegmentToCoupleTile(cs))
  implicit def tupleSegmentToTupleCoupleTile(ts: TupleSegment): TupleCoupleTile = Implicits.tupleTileToTupleCoupleTile(Implicits.tupleSegmentToTupleTile(ts))
  implicit def tupleSegmentToTupleSymTile(ts: TupleSegment): TupleSymTile = Implicits.tupleTileToTupleSymTile(Implicits.tupleSegmentToTupleTile(ts))
}

} // end of ImplicitsSyntax
