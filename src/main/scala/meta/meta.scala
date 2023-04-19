package io.github.memo33
package metarules.meta

import RotFlip._
import group._
import SymGroup._
import Quotient._

/** The content of this file makes the MetaRUL DSL syntax work. Changes in here
  * are unlikely to be necessary.
  *
  * The `Network` type should be provided by dependency injection by extending
  * from `Syntax` downstream.
  */
trait Syntax extends RuleGeneratorSyntax with RuleTransducerSyntax with ImplicitsSyntax {

type Network <: AbstractNetwork

trait AbstractNetwork { this: Network =>

  def typ: NetworkType

  def ~ (w: Int, n: Int, e: Int, s: Int): Segment =
    Segment(this, Flags(w, n, e, s, if (typ == Symmetrical) Flag.Bi else Flag.InOut))

  def ~ (flags: (Int, Int, Int, Int)): Segment =
    this ~ (flags._1, flags._2, flags._3, flags._4)

  // def ~ (flags: Flags): Segment = Segment(this, flags)

  def ~> (that: Network): CoupleNetwork = CoupleNetwork(this, that)
}

/** The function that maps `Tiles` (meta syntax) to `IdTiles` (IID, Rot, Flip).
  */
type IdResolver = PartialFunction[Tile, IdTile]

case class Segment(network: Network, flags: Flags) {
  def * (rf: RotFlip): Segment = copy(flags = flags * rf)
  override def toString: String = s"$network~$flags"

  def ~ (w: Int, n: Int, e: Int, s: Int): TupleSegment = TupleSegment(this, network~(w,n,e,s))
  def ~ (flags: (Int, Int, Int, Int)): TupleSegment = this ~ (flags._1, flags._2, flags._3, flags._4)
  def reverse: Segment = {
    assert(network.typ != AvenueLike || this != network~(Flags.SharedDiagLeft) && this != network~(Flags.SharedDiagRight),
      "reversing does not currently work for shared-tile diagonals") // TODO fix this
    copy(flags = flags.reverseFlags)
  }
  def projectLeft: Segment = copy(flags = flags.spinLeft)
  def projectRight: Segment = copy(flags = flags.spinRight)
}

private[meta] case class TupleSegment(seg1: Segment, seg2: Segment) {
  override def toString: String = s"$seg1~${seg2.flags}"
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

sealed trait SymTile extends TileLike {
  def symmetries: SymGroup
  def representations: Quotient = symmetries.quotient
  def * (rf: RotFlip): SymTile

  def toIdSymTile(resolve: Tile => IdTile): IdSymTile
}

case class Tile(segs: Set[Segment]) extends SymTile {
  lazy val symmetries: SymGroup = SymGroup.ofTile(this)
  def * (rf: RotFlip): Tile = copy(segs = segs map (_ * rf))
  def & (seg: Segment) = copy(segs = segs + seg)
  def & (tseg: TupleSegment): TupleTile = TupleTile(this & tseg.seg1, this & tseg.seg2)
  def & (cseg: CoupleSegment): CoupleTile = CoupleTile(this & cseg.seg1, this & cseg.seg2)
  def & (tcseg: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(this & tcseg.cseg1, this & tcseg.cseg2)
  def | (that: Tile): Rule.PartialRule2[Tile, Rule[Tile]] = {
    new Rule.PartialRule2(Rule.newBuilder[Tile] += this += that)
  }
  def | (that: TupleTile): Rule.PartialRule2[TupleTile, (Rule[Tile], Rule[Tile])] =
    TupleTile(this, this) | that
  def | (that: CoupleTile): Rule[Tile] = Rule(this, that.tile1, this, that.tile2)
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = (this | that.ctile1, this | that.ctile2)
  def | (cs: CoupleSegment): Rule[Tile] = this | Implicits.coupleSegmentToCoupleTile(cs)
  def | (that: IdSymTile): Rule.PartialRule2[SymTile, Rule[SymTile]] = {
    new Rule.PartialRule2(Rule.newBuilder[SymTile] += this += that)
  }
  override def toString = segs.mkString(" & ")

  def toIdSymTile(resolve: Tile => IdTile) = new IdSymTile(symmetries, resolve(this))
}
object Tile {
  def apply(seg: Segment): Tile = Tile(Set(seg))
}

private[meta] case class TupleSymTile(tile1: SymTile, tile2: SymTile) extends TupleTileLike[SymTile]

private[meta] case class TupleTile(tile1: Tile, tile2: Tile) extends TupleTileLike[Tile] {
  def & (seg: Segment) = copy(tile1 = tile1 & seg, tile2 = tile2 & seg)
  def & (tseg: TupleSegment) = copy(tile1 = tile1 & tseg.seg1, tile2 = tile2 & tseg.seg2)
  def & (cseg: CoupleSegment): TupleCoupleTile = TupleCoupleTile(tile1 & cseg, tile2 & cseg)
  def & (tcs: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(tile1 & tcs.cseg1, tile2 & tcs.cseg2)
  def | (that: TupleTile): Rule.PartialRule2[TupleTile, (Rule[Tile], Rule[Tile])] = {
    new Rule.PartialRule2(new Rule.TupleRuleBuilder[Tile, TupleTile]() += this += that)
  }
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = Implicits.tupleTileToTupleCoupleTile(this) | that
  def | (that: IdSymTile): Rule.PartialRule2[TupleSymTile, (Rule[SymTile], Rule[SymTile])] = {
    new Rule.PartialRule2(new Rule.TupleRuleBuilder[SymTile, TupleSymTile] += TupleSymTile(tile1, tile2) += TupleSymTile(that, that))
  }
}

private[meta] case class CoupleTile(tile1: Tile, tile2: Tile) extends TileLike {
  def & (seg: Segment) = copy(tile1 = tile1 & seg, tile2 = tile2 & seg)
  def & (cs: CoupleSegment) = copy(tile1 = tile1 & cs.seg1, tile2 = tile2 & cs.seg2)
  def & (ts: TupleSegment): TupleCoupleTile = TupleCoupleTile(this & ts.seg1, this & ts.seg2)
  def & (tcs: TupleCoupleSegment): TupleCoupleTile = TupleCoupleTile(this & tcs.cseg1, this & tcs.cseg2)
  def | (that: Tile): Rule[Tile] = Rule(this.tile1, that, this.tile2, that)
  def | (that: CoupleTile): Rule[Tile] = Rule(this.tile1, that.tile1, this.tile2, that.tile2)
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = Implicits.coupleTileToTupleCoupleTile(this) | that
  def | (cs: CoupleSegment): Rule[Tile] = this | Implicits.coupleSegmentToCoupleTile(cs)
  def | (that: IdSymTile): Rule[SymTile] = Rule(this.tile1, that, this.tile2, that)
}

private[meta] case class TupleCoupleTile(ctile1: CoupleTile, ctile2: CoupleTile) extends TileLike {
  def & (seg: Segment) = copy(ctile1 = ctile1 & seg, ctile2 = ctile2 & seg)
  def & (cs: CoupleSegment) = copy(ctile1 = ctile1 & cs, ctile2 = ctile2 & cs)
  def & (ts: TupleSegment) = copy(ctile1 = ctile1 & ts.seg1, ctile2 = ctile2 & ts.seg2)
  def & (tcs: TupleCoupleSegment) = copy(ctile1 = ctile1 & tcs.cseg1, ctile2 = ctile2 & tcs.cseg2)
  def | (that: TupleCoupleTile): (Rule[Tile], Rule[Tile]) = (this.ctile1 | that.ctile1, this.ctile2 | that.ctile2)
  def | (that: IdSymTile): (Rule[SymTile], Rule[SymTile]) = (this.ctile1 | that, this.ctile2 | that)
}

object IdTile {
  /** This constructor can be used to connect metarule syntax to ordinary RUL2 code.
    */
  def apply(id: Int, rf: RotFlip, symmetries: SymGroup): IdSymTile = {
    new IdSymTile(symmetries, internal.IdTile(id, rf))
  }
  def apply(id: Int, rf: RotFlip, symmetries: SymGroup, mappedRepr: Quotient => Set[RotFlip]): IdSymTile = {
    new IdSymTile(symmetries, internal.IdTile(id, rf, mappedRepr))
  }
  def apply(idTile: IdTile, symmetries: SymGroup): IdSymTile = {
    new IdSymTile(symmetries, idTile)
  }
  def apply(id: Int, rot: Int, flip: Int, symmetries: SymGroup): IdSymTile = {
    new IdSymTile(symmetries, internal.IdTile(id, RotFlip(rot, flip)))
  }
  def apply(id: Int, rot: Int, flip: Int): IdTile = {
    internal.IdTile(id, RotFlip(rot, flip))
  }
  def apply(id: Int, rf: RotFlip, mappedRepr: Quotient => Set[RotFlip] = identity): IdTile = {
    internal.IdTile(id, rf, mappedRepr)
  }
}

private[meta] class IdSymTile(val symmetries: SymGroup, idTile: IdTile) extends SymTile {
  def id: Int = idTile.id
  def rf: RotFlip = idTile.rf
  def repr: Set[RotFlip] = idTile.mappedRepr(symmetries.quotient)
  override def toString = s"$idTile[${symmetries.name}]"

  def * (rf: RotFlip) = new IdSymTile(symmetries * rf, idTile * rf)

  def | (that: Tile): Rule.PartialRule2[SymTile, Rule[SymTile]] = {
    new Rule.PartialRule2(Rule.newBuilder[SymTile] += this += that)
  }
  def | (that: IdSymTile): Rule.PartialRule2[SymTile, Rule[SymTile]] = {
    new Rule.PartialRule2(Rule.newBuilder[SymTile] += this += that)
  }
  def | (that: TupleTile): Rule.PartialRule2[TupleSymTile, (Rule[SymTile], Rule[SymTile])] = {
    new Rule.PartialRule2(new Rule.TupleRuleBuilder[SymTile, TupleSymTile] += TupleSymTile(this, this) += TupleSymTile(that.tile1, that.tile2))
  }
  def | (that: CoupleTile): Rule[SymTile] = Rule(this, that.tile1, this, that.tile2)
  def | (that: TupleCoupleTile): (Rule[SymTile], Rule[SymTile]) = (this | that.ctile1, this | that.ctile2)
  def | (cs: CoupleSegment): Rule[SymTile] = this | Implicits.coupleSegmentToCoupleTile(cs)

  def toIdSymTile(resolve: Tile => IdTile): this.type = this
}

}  // end of Syntax


private[meta] sealed trait TileLike

private[meta] sealed trait TupleTileLike[A] extends TileLike {
  def tile1: A
  def tile2: A
}

package internal {  // <-- to avoid name conflicts

  /** Constructors are provided by Syntax#IdTile object. */
  case class IdTile(id: Int, rf: RotFlip, mappedRepr: Quotient => Set[RotFlip] = identity) extends TileLike {
    override def toString = f"0x$id%08X,${rf.rot},${rf.flip}"
    def * (rf: RotFlip) = copy(rf = this.rf * rf)

    override def equals(any: Any): Boolean = any match {
      case IdTile(id, rf, mr) => id == this.id && rf == this.rf
      case _ => false
    }
    override def hashCode: Int = id ^ rf.hashCode
  }
}

import scala.reflect.ClassTag
import scala.collection.mutable.{Builder, ArrayBuilder}
import scala.collection.immutable.ArraySeq

class Rule[+A <: TileLike] private (private val ts: ArraySeq[A]) {
  def apply(i: Int): A = ts(i)
  def map[B <: TileLike](f: A => B): Rule[B] = new Rule(ts.map(f))
  def forall(p: A => Boolean): Boolean = ts.forall(p)
  def exists(p: A => Boolean): Boolean = ts.exists(p)
  def foreach[U](f: A => U): Unit = ts.foreach(f)
  override def toString: String = ts.mkString("Rule( ", " | ", " )")
  override def equals(that: Any): Boolean = that match {
    case that: Rule[_] => this.ts == that.ts
    case _ => false
  }
  override def hashCode: Int = ts.hashCode
}

object Rule {

  case object CopyTile

  def apply[A <: TileLike : ClassTag](a: A, b: A, c: A, d: A) = new Rule(ArraySeq(a, b, c, d))

  def apply(
    a: Int, ar: Byte, af: Byte,
    b: Int, br: Byte, bf: Byte,
    c: Int, cr: Byte, cf: Byte,
    d: Int, dr: Byte, df: Byte): Rule[IdTile] = {
      import internal.IdTile
      Rule(IdTile(a, RotFlip(ar, af)), IdTile(b, RotFlip(br, bf)), IdTile(c, RotFlip(cr, cf)), IdTile(d, RotFlip(dr, df)))
  }

  abstract class RuleBuilderLike[B <: TileLike : ClassTag, C] extends Builder[B, C] {
    private[Rule] var arr = new Array[B](4)
    private[this] var i = 0
    def addOne(elem: B): this.type = {
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
    def result(): Rule[B] = new Rule(arr.to(ArraySeq))
  }

  // A = Tile, B = TupleTile or A = SymTile, B = TupleSymTile
  private[meta] class TupleRuleBuilder[A <: TileLike : ClassTag, B <: TupleTileLike[A] : ClassTag]() extends RuleBuilderLike[B, (Rule[A], Rule[A])] {
    def result(): (Rule[A], Rule[A]) = {
      (Rule(arr(0).tile1, arr(1).tile1, arr(2).tile1, arr(3).tile1), Rule(arr(0).tile2, arr(1).tile2, arr(2).tile2, arr(3).tile2))
    }
  }

  def newBuilder[B <: TileLike : ClassTag]: RuleBuilder[B] = new RuleBuilder()

  private[meta] class PartialRule2[A <: TileLike : ClassTag, C] private[meta] (b: RuleBuilderLike[A, C]) {
    def | (tile: A): PartialRule3[A, C] = new PartialRule3(b += tile)
    def | (tile: Rule.CopyTile.type): PartialRule3[A, C] = new PartialRule3(b += b.arr(0))
  }
  private[meta] class PartialRule3[A <: TileLike : ClassTag, C] private[meta] (b: RuleBuilderLike[A, C]) {
    def | (tile: A): C = (b += tile).result()
    def | (tile: Rule.CopyTile.type): C = (b += b.arr(1)).result()
  }
}
