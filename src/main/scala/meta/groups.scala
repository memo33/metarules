package meta

import RotFlip.GroupElement

/** The eight rotations of a tile, e.g. in an override rule.
  */
class RotFlip private (val rot: Int, val flip: Int) extends RotFlip.Val {

  /** Group multiplication from *left*, but written from *right*.
    */
  def * (that: GroupElement): GroupElement = {
    val r = (this.rot + (if (this.flip == 1) -1 else 1) * that.rot) & 0x3
    val f = if (this.flip != that.flip) 1 else 0
    RotFlip.withRotFlip(r, f)
  }

  /** Group division from *left*, but written from *right* (multiplication
    * with right operand inverted).
    */
  def / (that: GroupElement): GroupElement = {
    val r = (this.rot + (if (this.flip != that.flip) 1 else -1) * that.rot) & 0x3
    val f = if (this.flip != that.flip) 1 else 0
    RotFlip.withRotFlip(r, f)
  }

  def ∈ (g: Seq[RotFlip]): Boolean = g.contains(this)

  override def toString = "(" + rot + "," + flip + ")"
  def flipped: Boolean = flip != 0

}

/** The eight rotations of a tile, e.g. in an override rule.
  */
object RotFlip extends scalaenum.Enum { // was previously called GroupElement

  type Value = RotFlip
  type GroupElement = RotFlip

  val R0F0 = new RotFlip(0,0)
  val R1F0 = new RotFlip(1,0)
  val R2F0 = new RotFlip(2,0)
  val R3F0 = new RotFlip(3,0)
  val R0F1 = new RotFlip(0,1)
  val R3F1 = new RotFlip(3,1)
  val R2F1 = new RotFlip(2,1)
  val R1F1 = new RotFlip(1,1)

  val elements = IndexedSeq(R0F0, R1F0, R2F0, R3F0, R0F1, R3F1, R2F1, R1F1)
  private val elems = Array(R0F0, R1F0, R2F0, R3F0, R0F1, R1F1, R2F1, R3F1) // sic! different order to simplify 'withRotFlip'

  private def withRotFlip(rot: Int, flip: Int): GroupElement = elems(rot + flip * 4)

  def apply(rot: Int, flip: Int): GroupElement = {
    require(rot >= 0 && rot < 4 && (flip == 0 || flip == 1))
    withRotFlip(rot, flip)
  }
}

sealed trait Group extends Seq[GroupElement] { this: Group.ValName =>
  protected val rep: Seq[GroupElement]

  def apply(idx: Int): GroupElement = rep(0)
  def iterator: Iterator[GroupElement] = rep.iterator
  def length: Int = rep.length
  override def stringPrefix = name

}

object Group {

  // to avoid clashing of mixed in toStrings
  private[meta] trait ValName {
    def name: String = super.toString
  }

  class QuotientGroup private (protected val rep: Seq[GroupElement])
    extends QuotientGroup.Val with ValName with Group
  object QuotientGroup extends scalaenum.Enum {
    import RotFlip._

    type Value = QuotientGroup

    private def Val(r: Seq[Tuple2[Int, Int]]) =
      new QuotientGroup(r.map(tup => RotFlip(tup._1, tup._2)))

    val Cyc1  = Val(Seq((0,0)))
    val Cyc2A = Val(Seq((0,0), (1,0)))
    val Cyc2B = Val(Seq((0,0), (0,1)))
    val Cyc4  = Val(Seq((0,0), (1,0), (2,0), (3,0)))
    val Dih2  = Val(Seq((0,0), (1,0), (0,1), (3,1)))
    val Dih4  = Val(Seq((0,0), (1,0), (2,0), (3,0), (0,1), (3,1), (2,1), (1,1)))

  }

  class SymGroup private (
    protected val rep: Seq[GroupElement],
    val quotient: QuotientGroup)
      extends SymGroup.Val with ValName with Group {
    import SymGroup._

    def sub(that: SymGroup): Boolean = this forall (that contains _)

    def * (g: GroupElement): SymGroup = {
      if (this < Cyc2B || this > Cyc2E) this
      else if (this == Cyc2B || this == Cyc2D) {
        if (this == Cyc2B ^ g.rot % 2 == 0) Cyc2D else Cyc2B
      } else {
        if (this == Cyc2C ^ (g.rot + g.flip) % 2 == 0) Cyc2E else Cyc2C
      }
    }
  }
  object SymGroup extends scalaenum.Enum {
    import RotFlip._

    type Value = SymGroup

    private def Val(r: Seq[Tuple2[Int, Int]], q: QuotientGroup) =
      new SymGroup(r.map(tup => RotFlip(tup._1, tup._2)), q)

    val Cyc1  = Val(Seq((0,0)), QuotientGroup.Dih4)
    val Cyc2A = Val(Seq((0,0), (2,0)), QuotientGroup.Dih2)
    val Cyc2B = Val(Seq((0,0), (0,1)), QuotientGroup.Cyc4)
    val Cyc2C = Val(Seq((0,0), (1,1)), QuotientGroup.Cyc4)
    val Cyc2D = Val(Seq((0,0), (2,1)), QuotientGroup.Cyc4)
    val Cyc2E = Val(Seq((0,0), (3,1)), QuotientGroup.Cyc4)
    val Cyc4  = Val(Seq((0,0), (1,0), (2,0), (3,0)), QuotientGroup.Cyc2B)
    val Dih2A = Val(Seq((0,0), (2,0), (0,1), (2,1)), QuotientGroup.Cyc2A)
    val Dih2B = Val(Seq((0,0), (2,0), (1,1), (3,1)), QuotientGroup.Cyc2A)
    val Dih4  = Val(Seq((0,0), (1,0), (2,0), (3,0), (0,1), (3,1), (2,1), (1,1)), QuotientGroup.Cyc1)

    def ofFlags(flags: Flags): SymGroup =
      ofImpl(rf => flags == flags * rf)
    def ofTile(tile: Tile): SymGroup =
      ofImpl(rf => tile.segs == tile.segs.map(_ * rf))

    private[this] def ofImpl(has: RotFlip => Boolean): SymGroup = {
      if (has(R2F0)) {
        if (has(R1F0)) {
          if (has(R0F1)) Dih4
          else Cyc4
        } else { // not R1F0
          if (has(R0F1)) Dih2A
          else if (has(R1F1)) Dih2B
          else Cyc2A
        }
      } else { // not R2F0
        if (has(R0F1)) Cyc2B
        else if (has(R1F1)) Cyc2C
        else if (has(R2F1)) Cyc2D
        else if (has(R3F1)) Cyc2E
        else Cyc1
      }
    }
  }

}
