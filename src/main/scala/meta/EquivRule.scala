package meta

import RotFlip._
import EquivRule._

/** A wrapper for Rules of IdTiles that respects the proper equivalence relation
  * of override rules in its `equals` and `hashCode` implementation. That means
  * two override rules are equivalent if they can be rotated and mirrored such
  * that they match, according to the following pattern:
  * A,0,0,B,0,0
  * A,2,1,B,2,1
  * B,2,0,A,2,0
  * B,0,1,A,0,1
  */
final class EquivRule(val rule: Rule[IdTile]) {

  override final def equals(any: Any): Boolean = any match {
    case that: EquivRule =>
      val rfh = rfHash(this.rule)
      if (rfh != rfHash(that.rule)) {
        false
      } else {
        @inline def same(mine: Int, yours: Int): Boolean = this.rule(mine).id == that.rule(yours).id
        if (isWeird(rfh)) { // the weird equiv classes have only 2 RotFlips (rather than 4) which is why this additional case necessary
          same(0, 0) && same(1, 1) || same(0, 1) && same(1, 0)
        } else if (swapped(this.rule) == swapped(that.rule)) {
          same(0, 0) && same(1, 1)
        } else {
          same(0, 1) && same(1, 0)
        }
      }
    case _ => false
  }

  override final def hashCode: Int = {
    val rfh = rfHash(rule)  // bounds: 0 <= rfh < 32
    val a = rule(0).id; val b = rule(1).id
    if (swapped(rule) || isWeird(rfh) && b < a) {
      idHash(b, a, rfh)
    } else {
      idHash(a, b, rfh)
    }
  }

  @inline private[this] def idHash(a: Int, b: Int, c: Int): Int = {
    // most ID information lies in bits 7-23 (17 bits), so prime should have
    // at least 32-17 = 15 bits so as to shift much of the information around
    val prime = 66403
    ((prime + a) * prime + b) * prime + c
  }

  override def toString: String = "EquivRule(" + rule + ")"
}

private object EquivRule {

  private[this] def smallest(a: RotFlip, b: RotFlip): (Int, Boolean) = {
    val (a2, b2, swapped) = Seq(
      (a, b, false),
      (a * R2F1, b * R2F1, false),
      (b * R2F0, a * R2F0, true),
      (b * R0F1, a * R0F1, true)) minBy { case (a,b,_) => a.id * 8 + b.id }
    (a2.id * 8 + b2.id, swapped)
  }

  private[this] val rfHashArray = Array.tabulate[Int](8, 8) { case (i, j) => smallest(RotFlip(i), RotFlip(j))._1 }
  private[this] val swappedArray = Array.tabulate[Boolean](8, 8) { case (i, j) => smallest(RotFlip(i), RotFlip(j))._2 }
  private[this] val equivClassSize = {
    val classes = rfHashArray.flatten.groupBy(identity)
    Array.tabulate[Int](32)(i => classes.getOrElse(i, Array.emptyIntArray).size)
  }

  @inline private def rfHash(rule: Rule[IdTile]): Int = rfHashArray(rule(0).rf.id)(rule(1).rf.id)
  @inline private def swapped(rule: Rule[IdTile]): Boolean = swappedArray(rule(0).rf.id)(rule(1).rf.id)
  @inline private def isWeird(rfh: Int): Boolean = equivClassSize(rfh) != 4
}
