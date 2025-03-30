package io.github.memo33
package metarules.meta

import group._, SymGroup._, Quotient._, RotFlip._
import Flag._

/** Network flags subject to the following conventions:
  * - flags ending in 1 or 3 are swapped when flipped (useful for curves),
  * - flags for symmetrical networks are non-negative,
  * - flags for asymmetrical networks can be negative; they represent a direction
  *   of travel (from negative to positive) and they change sign when flipped or reversed.
  */
object Flag {

  enum Kind {
    case Default, LeftSpin, RightSpin
  }

  sealed trait FlagManifest {
    def stringify(flag: Int): String
    def parse(number: Int): Int
    def flip(flag: Int): Int
    def reverse(flag: Int): Int
    protected def suffix: String = ""
    def kind: Kind = Kind.Default
  }
  sealed trait BiManifest extends FlagManifest {
    def stringify(flag: Int) = if (flag == 0) "0" else flag.toString + suffix
    def parse(number: Int) = number.abs
    def reverse(flag: Int) = flag
    def flip(flag: Int) = (flag % 10) match {
      case 3 => flag - 2
      case 1 => flag + 2
      case _ => flag
    }
  }
  sealed trait InOutManifest extends FlagManifest {
    def stringify(flag: Int) = if (flag == 0) "0" else (if (flag > 0) "+" + flag.toString else flag.toString) + suffix
    def parse(number: Int) = number
    def reverse(flag: Int) = -flag
    def flip(flag: Int) = (flag % 10) match {
      case  3 => -(flag - 2)
      case  1 => -(flag + 2)
      case -1 => -(flag - 2)
      case -3 => -(flag + 2)
      case  _ => -flag
    }
  }

  object Bi extends BiManifest
  object InOut extends InOutManifest
  object LeftSpinBi     extends BiManifest    { override def suffix = "L"; override def kind: Kind = Kind.LeftSpin }
  object LeftSpinInOut  extends InOutManifest { override def suffix = "L"; override def kind: Kind = Kind.LeftSpin }
  object RightSpinBi    extends BiManifest    { override def suffix = "R"; override def kind: Kind = Kind.RightSpin }
  object RightSpinInOut extends InOutManifest { override def suffix = "R"; override def kind: Kind = Kind.RightSpin }
}

/** An implementation of `IndexedSeq[Int]` that is limited to length 4.
  */
class Flags private (fs: Array[Int], val manifest: FlagManifest, off: Int = 0) extends IndexedSeq[Int] {

  def apply(i: Int): Int = {
    require(i >= 0 && i < 4)
    fs((i + off) % 4)
  }
  def length: Int = 4
  // override def stringPrefix: String = "Flags"
  override def toString: String = this.map(manifest.stringify).mkString("(", ",", ")")
  override def canEqual(obj: Any): Boolean = obj match {
    case other: Flags => other.manifest == this.manifest
    case _ => false
  }

  def * (rf: RotFlip): Flags = {
    val rotated = if (rf.rot == 0) this else new Flags(fs, manifest, (off + 4 - rf.rot) % 4)
    if (rf.flipped)
      new Flags(Array(manifest.flip(rotated(2)), manifest.flip(rotated(1)), manifest.flip(rotated(0)), manifest.flip(rotated(3))), manifest)
    else
      rotated
  }

  // Note that the method `reverse` inherited from `IndexedSeq` changes the order of the flags instead.
  def reverseFlags: Flags = new Flags(fs.map(manifest.reverse), manifest, off)

  lazy val symmetries: SymGroup = SymGroup.ofFlags(this)
  def representations: Quotient = symmetries.quotient

  def spinLeft: Flags = {
    val newMf = manifest match {
      case Bi => LeftSpinBi
      case InOut => LeftSpinInOut
      case _ => throw new UnsupportedOperationException(s"flags $this have already been projected and cannot be projected further")
    }
    new Flags(fs, newMf, off)
  }
  def spinRight: Flags = {
    val newMf = manifest match {
      case Bi => RightSpinBi
      case InOut => RightSpinInOut
      case _ => throw new UnsupportedOperationException(s"flags $this have already been projected and cannot be projected further")
    }
    new Flags(fs, newMf, off)
  }
}

/** Defines a few common Int tuples for use as Flags.
  */
object Flags {
  type IntFlags = (Int, Int, Int, Int)

  def apply(w: Int, n: Int, e: Int, s: Int, manifest: FlagManifest): Flags = new Flags(Array(w, n, e, s).map(manifest.parse), manifest)
  def apply(wnes: IndexedSeq[Int], manifest: FlagManifest): Flags = new Flags(wnes.toArray.map(manifest.parse), manifest)

  // use carefully and only with directed flags
  def reverseIntFlags(flags: IntFlags): IntFlags = flags match { case (w, n, e, s) => (-w, -n, -e, -s) }

  // orth
  val WE = (-2,0,2,0)
  val NS = (0,-2,0,2)
  val EW = (2,0,-2,0)
  val SN = (0,2,0,-2)
  // orth stub
  val WC = (-2,0,0,0)
  val NC = (0,-2,0,0)
  val EC = (0,0,-2,0)
  val SC = (0,0,0,-2)
  val CW = (2,0,0,0)
  val CN = (0,2,0,0)
  val CE = (0,0,2,0)
  val CS = (0,0,0,2)
  // diag
  val ES = (0,0,-1,3)
  val SW = (3,0,0,-1)
  val WN = (-1,3,0,0)
  val NE = (0,-1,3,0)
  val SE = (0,0,1,-3)
  val WS = (-3,0,0,1)
  val NW = (1,-3,0,0)
  val EN = (0,1,-3,0)
  // diag stub
  val ESC = (0,0,-1,0)
  val SWC = (0,0,0,-1)
  val WNC = (-1,0,0,0)
  val NEC = (0,-1,0,0)
  val CSE = (0,0,1,0)
  val CWS = (0,0,0,1)
  val CNW = (1,0,0,0)
  val CEN = (0,1,0,0)
  val SEC = (0,0,0,-3)
  val WSC = (-3,0,0,0)
  val NWC = (0,-3,0,0)
  val ENC = (0,0,-3,0)
  val CES = (0,0,0,3)
  val CSW = (3,0,0,0)
  val CWN = (0,3,0,0)
  val CNE = (0,0,3,0)
  // avenue-like diagonals
  val SharedDiagLeft = (-3,1,-3,1)
  val SharedDiagRight = (1,-3,1,-3)
}
