package metarules.meta

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
}

trait LowPriorityImplicits {
  import scala.language.implicitConversions
  implicit def segmentToTupleTile(seg: Segment): TupleTile = Implicits.tileToTupleTile(Implicits.segmentToTile(seg))
  implicit def segmentToCoupleTile(seg: Segment): CoupleTile = Implicits.tileToCoupleTile(Implicits.segmentToTile(seg))
  implicit def segmentToTupleCoupleTile(seg: Segment): TupleCoupleTile = Implicits.tileToTupleCoupleTile(Implicits.segmentToTile(seg))
  implicit def coupleSegmentToTupleCoupleTile(cs: CoupleSegment): TupleCoupleTile = Implicits.coupleTileToTupleCoupleTile(Implicits.coupleSegmentToCoupleTile(cs))
  implicit def tupleSegmentToTupleCoupleTile(ts: TupleSegment): TupleCoupleTile = Implicits.tupleTileToTupleCoupleTile(Implicits.tupleSegmentToTupleTile(ts))
}
