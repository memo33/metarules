package meta

import RotFlip._
import Group._
import SymGroup._
import QuotientGroup._
import Network.Network

/* The content of this file makes the MetaRUL DSL syntax work. Changes in here
 * are unlikely to be necessary.
 */

case class Segment(network: Network, flags: Flags) {
  def * (rf: RotFlip): Segment = copy(flags = flags * rf)
  override def toString: String = network + "~" + flags

  def ~ (w: Int, n: Int, e: Int, s: Int): TupleSegment = TupleSegment(this, network~(w,n,e,s))
  def ~ (flags: (Int, Int, Int, Int)): TupleSegment = this ~ (flags._1, flags._2, flags._3, flags._4)
  def reverse: Segment = {
    assert(network.typ != Network.AvenueLike || this != network~(Flags.SharedDiagLeft) && this != network~(Flags.SharedDiagRight),
      "reversing does not currently work for shared-tile diagonals") // TODO fix this
    copy(flags = Flags(flags(0).reverse, flags(1).reverse, flags(2).reverse, flags(3).reverse)) // TODO implement CanBuildFrom
  }
}

private[meta] case class TupleSegment(seg1: Segment, seg2: Segment) {
  override def toString: String = seg1 + "~" + seg2.flags
}

private[meta] case class CoupleNetwork(network1: Network, network2: Network) {
  override def toString: String = "(" + network1 + " ~> " + network2 + ")"
  def ~ (w: Int, n: Int, e: Int, s: Int): CoupleSegment = CoupleSegment(network1~(w,n,e,s), network2~(w,n,e,s))
  def ~ (flags: (Int, Int, Int, Int)): CoupleSegment = CoupleSegment(network1~flags, network2~flags)
}

private[meta] case class CoupleSegment(seg1: Segment, seg2: Segment) {
  override def toString: String = "(" + seg1 + " ~> " + seg2 + ")"
  def ~ (w: Int, n: Int, e: Int, s: Int): TupleCoupleSegment = TupleCoupleSegment(this, CoupleSegment(seg1.network~(w,n,e,s), seg2.network~(w,n,e,s)))
  def ~ (flags: (Int, Int, Int, Int)): TupleCoupleSegment = this ~ (flags._1, flags._2, flags._3, flags._4)
}

private[meta] case class TupleCoupleSegment(cseg1: CoupleSegment, cseg2: CoupleSegment)

private[meta] sealed trait TileLike

private[meta] sealed trait SymTile extends TileLike {
  def symmetries: SymGroup
  def representations: QuotientGroup = symmetries.quotient
}

case class Tile(segs: Set[Segment]) extends SymTile {
  lazy val symmetries: SymGroup = SymGroup.ofTile(this)
  def * (rf: RotFlip): Tile = copy(segs = segs map (_ * rf))
  def & (seg: Segment) = copy(segs = segs + seg)
  def & (tseg: TupleSegment): TupleTile = TupleTile(this & tseg.seg1, this & tseg.seg2)
  def & (cseg: CoupleSegment): CoupleTile = CoupleTile(this & cseg.seg1, this & cseg.seg2)
  def & (tcseg: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(this & tcseg.cseg1, this & tcseg.cseg2)
  def | (that: Tile): Rule.PartialRule2[Tile, Rule[Tile]] = {
    val b = Rule.newBuilder[Tile]
    b += this
    b += that
    new Rule.PartialRule2(b)
  }
  def | (that: TupleTile): Rule.PartialRule2[TupleTile, (Rule[Tile], Rule[Tile])] =
    TupleTile(this, this) | that
  def | (that: CoupleTile): Rule[Tile] = Rule(this, that.tile1, this, that.tile2)
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = (this | that.ctile1, this | that.ctile2)
  def | (cs: CoupleSegment): Rule[Tile] = this | Implicits.coupleSegmentToCoupleTile(cs)
  override def toString = segs.mkString(" & ")
}

private[meta] case class TupleTile(tile1: Tile, tile2: Tile) extends TileLike {
  def & (seg: Segment) = copy(tile1 = tile1 & seg, tile2 = tile2 & seg)
  def & (tseg: TupleSegment) = copy(tile1 = tile1 & tseg.seg1, tile2 = tile2 & tseg.seg2)
  def & (cseg: CoupleSegment): TupleCoupleTile = TupleCoupleTile(tile1 & cseg, tile2 & cseg)
  def & (tcs: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(tile1 & tcs.cseg1, tile2 & tcs.cseg2)
  def | (that: TupleTile): Rule.PartialRule2[TupleTile, (Rule[Tile], Rule[Tile])] = {
    val b = new Rule.TupleRuleBuilder()
    b += this
    b += that
    new Rule.PartialRule2(b)
  }
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = Implicits.tupleTileToTupleCoupleTile(this) | that
}

private[meta] case class CoupleTile(tile1: Tile, tile2: Tile) extends TileLike {
  def & (seg: Segment) = copy(tile1 = tile1 & seg, tile2 = tile2 & seg)
  def & (cs: CoupleSegment) = copy(tile1 = tile1 & cs.seg1, tile2 = tile2 & cs.seg2)
  def & (ts: TupleSegment): TupleCoupleTile = TupleCoupleTile(this & ts.seg1, this & ts.seg2)
  def & (tcs: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(this & tcs.cseg1, this & tcs.cseg2)
  def | (that: Tile): Rule[Tile] = Rule(this.tile1, that, this.tile2, that)
  def | (that: CoupleTile): Rule[Tile] = Rule(this.tile1, that.tile1, this.tile2, that.tile2)
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = Implicits.coupleTileToTupleCoupleTile(this) | that
}

private[meta] case class TupleCoupleTile(ctile1: CoupleTile, ctile2: CoupleTile) extends TileLike {
  def & (seg: Segment) = copy(ctile1 = ctile1 & seg, ctile2 = ctile2 & seg)
  def & (cs: CoupleSegment) = copy(ctile1 = ctile1 & cs, ctile2 = ctile2 & cs)
  def & (ts: TupleSegment) = copy(ctile1 = ctile1 & ts.seg1, ctile2 = ctile2 & ts.seg2)
  def & (tcs: TupleCoupleSegment) = copy(ctile1 = ctile1 & tcs.cseg1, ctile2 = ctile2 & tcs.cseg2)
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = (this.ctile1 | that.ctile1, this.ctile2 | that.ctile2)
}

object Tile {
  def apply(seg: Segment): Tile = Tile(Set(seg))

  private def projectTla(t: Tile, p: Flags => Flags): Tile = t.copy(segs =
    t.segs.map(s => if (!s.network.isTla) s else s.copy(flags = p(s.flags)))
    )
  /*private*/ val projectLeft = (t: Tile) => projectTla(t, _.makeLeftHeaded)
  /*private*/ val projectRight = (t: Tile) => projectTla(t, _.makeRightHeaded)

  case object CopyTile
}

case class IdTile(id: Int, rf: RotFlip, mappedRepr: QuotientGroup => Seq[RotFlip] = identity) extends TileLike {
  override def toString = f"0x$id%08X,${rf.rot},${rf.flip}"
  def * (rf: RotFlip) = copy(rf = this.rf * rf)

  override def equals(any: Any): Boolean = any match {
    case IdTile(id, rf, mr) => id == this.id && rf == this.rf
    case _ => false
  }
  override def hashCode: Int = id ^ rf.hashCode
}

private[meta] class IdSymTile(symTile: SymTile, idTile: IdTile) extends SymTile {
  def symmetries: SymGroup = symTile.symmetries
  def id: Int = idTile.id
  def rf: RotFlip = idTile.rf
  def repr: Seq[RotFlip] = idTile.mappedRepr(symmetries.quotient)
}


import scala.reflect.ClassTag
import scala.collection.IndexedSeqOptimized
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder, ArrayBuilder}

class Rule[+A <: TileLike : ClassTag] private (ts: Array[A]) extends IndexedSeq[A] with IndexedSeqOptimized[A, Rule[A]] {
  def apply(i: Int): A = ts(i)
  def length: Int = 4
  override protected[this] def newBuilder = Rule.newBuilder[A]
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Rule[_]]
  //override def stringPrefix: String = "Rule"
  override def toString: String = ts.mkString("Rule( ", " | ", " )")
}

object Rule {

  def apply[A <: TileLike : ClassTag](a: A, b: A, c: A, d: A) = new Rule(Array(a, b, c, d))

  abstract class RuleBuilderLike[B <: TileLike : ClassTag, C] extends Builder[B, C] {
    private[Rule] var arr = new Array[B](4)
    private[this] var i = 0
    def += (elem: B): this.type = {
      arr(i) = elem
      i += 1
      this
    }
    def clear(): Unit = {
      arr = new Array[B](4)
      i = 0
    }
  }

  class RuleBuilder[B <: TileLike : ClassTag] private[Rule] () extends RuleBuilderLike[B, Rule[B]] {
    def result(): Rule[B] = new Rule(arr)
  }

  private[meta] class TupleRuleBuilder() extends RuleBuilderLike[TupleTile, (Rule[Tile], Rule[Tile])] {
    def result(): (Rule[Tile], Rule[Tile]) = {
      (Rule(arr(0).tile1, arr(1).tile1, arr(2).tile1, arr(3).tile1), Rule(arr(0).tile2, arr(1).tile2, arr(2).tile2, arr(3).tile2))
    }
  }

  def newBuilder[B <: TileLike : ClassTag]: RuleBuilder[B] = new RuleBuilder()

  implicit def cbf[B <: TileLike : ClassTag]: CanBuildFrom[Seq[_], B, Rule[B]] = new CanBuildFrom[Seq[_], B, Rule[B]] {
    def apply(): Builder[B, Rule[B]] = newBuilder
    def apply(from: Seq[_]): Builder[B, Rule[B]] = newBuilder
  }

  private[meta] class PartialRule2[A <: TileLike : ClassTag, C] private[meta] (b: RuleBuilderLike[A, C]) {
    def | (tile: A): PartialRule3[A, C] = new PartialRule3(b += tile)
    def | (tile: Tile.CopyTile.type): PartialRule3[A, C] = new PartialRule3(b += b.arr(0))
  }
  private[meta] class PartialRule3[A <: TileLike : ClassTag, C] private[meta] (b: RuleBuilderLike[A, C]) {
    def | (tile: A): C = (b += tile).result()
    def | (tile: Tile.CopyTile.type): C = (b += b.arr(1)).result()
  }
}
