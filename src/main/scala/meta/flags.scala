package metarules.meta

import Group._, SymGroup._, QuotientGroup._, RotFlip._
import Flag._

/** Represents a single flag commonly used in SC4 RULs, tough with slight
  * variations.
  */
class Flag private (initFlip: => Flag, initRev: => Flag, override val toString: String, project: CommonFlags => Flag) { // lazy instantiation to handle mutual references
  lazy val flip: Flag = initFlip
  lazy val reverse: Flag = initRev

  // this is to make TLA networks work--it is likely to change
  def makeLeftHeaded: Flag = this match {
    case Zero => this
    case f: CommonFlags#NestedFlag => f.outer match {
      case Bi => project(LeftHeadedBi)
      case In => project(LeftHeadedIn)
      case Out => project(LeftHeadedOut)
    }
  }
  def makeRightHeaded: Flag = this match {
    case Zero => this
    case f: CommonFlags#NestedFlag => f.outer match {
      case Bi => project(RightHeadedBi)
      case In => project(RightHeadedIn)
      case Out => project(RightHeadedOut)
    }
  }
}

/** If new flags shall be defined, they need to be defined here, in the
  * `CommonFlags` trait of this `Flag` object. You will also have to add the Int
  * representations to the parsing methods in here.
  */
object Flag {

  object Kind extends Enumeration {
    val Default, LeftHeaded, RightHeaded = Value
  }

  val Zero: Flag = new Flag(Zero, Zero, "0", _ => Zero)
  val Four: Flag = new Flag(Four, Four, "4", _ => Four)

  trait CommonFlags {
    private[Flag] class NestedFlag(initFlip: => Flag, initRev: => Flag, i: Int, project: Flag.CommonFlags => Flag) extends Flag(initFlip, initRev, stringPrefix + i + stringSuffix, project) {
      def outer: CommonFlags = CommonFlags.this
    }

    val flipFlags: CommonFlags
    val revFlags: CommonFlags
    def stringPrefix: String = ""
    def stringSuffix: String = ""

    val Orth: Flag = new NestedFlag(flipFlags.Orth, revFlags.Orth, 2, _.Orth)
    val DiagLeft: Flag = new NestedFlag(flipFlags.DiagRight, revFlags.DiagLeft, 1, _.DiagLeft)
    val DiagRight: Flag = new NestedFlag(flipFlags.DiagLeft, revFlags.DiagRight, 3, _.DiagRight)
    val BlendLeft: Flag = new NestedFlag(flipFlags.BlendRight, revFlags.BlendLeft, 11, _.BlendLeft)
    val BlendRight: Flag = new NestedFlag(flipFlags.BlendLeft, revFlags.BlendRight, 13, _.BlendRight)
    val ExtBlendLeft: Flag = new NestedFlag(flipFlags.ExtBlendRight, revFlags.ExtBlendLeft, 111, _.ExtBlendLeft)
    val ExtBlendRight: Flag = new NestedFlag(flipFlags.ExtBlendLeft, revFlags.ExtBlendRight, 113, _.ExtBlendRight)
    val BlendLeftR1: Flag = new NestedFlag(flipFlags.BlendRightR1, revFlags.BlendLeftR1, 121, _.BlendLeftR1)
    val BlendRightR1: Flag = new NestedFlag(flipFlags.BlendLeftR1, revFlags.BlendRightR1, 123, _.BlendRightR1)
    val FlexFly01L: Flag = new NestedFlag(flipFlags.FlexFly01R, revFlags.FlexFly01L, 211, _.FlexFly01L)
    val FlexFly01R: Flag = new NestedFlag(flipFlags.FlexFly01L, revFlags.FlexFly01R, 213, _.FlexFly01R)
    val FlexFly12L: Flag = new NestedFlag(flipFlags.FlexFly12R, revFlags.FlexFly12L, 221, _.FlexFly12L)
    val FlexFly12R: Flag = new NestedFlag(flipFlags.FlexFly12L, revFlags.FlexFly12R, 223, _.FlexFly12R)
    val FlexFly34L: Flag = new NestedFlag(flipFlags.FlexFly34R, revFlags.FlexFly34L, 231, _.FlexFly34L)
    val FlexFly34R: Flag = new NestedFlag(flipFlags.FlexFly34L, revFlags.FlexFly34R, 233, _.FlexFly34R)
    val FlexFly45L: Flag = new NestedFlag(flipFlags.FlexFly45R, revFlags.FlexFly45L, 241, _.FlexFly45L)
    val FlexFly45R: Flag = new NestedFlag(flipFlags.FlexFly45L, revFlags.FlexFly45R, 243, _.FlexFly45R)
    // --- add new flags here ---
  }

  object Bi extends CommonFlags { val flipFlags = this; val revFlags = this }
  object In extends CommonFlags { val flipFlags = Out; val revFlags = Out; override def stringPrefix = "-" }
  object Out extends CommonFlags { val flipFlags = In; val revFlags = In; override def stringPrefix = "+" }
  // TODO check revFlags
  object LeftHeadedBi extends CommonFlags { val flipFlags = LeftHeadedBi; val revFlags = LeftHeadedBi; override def stringSuffix = "L" }
  object RightHeadedBi extends CommonFlags { val flipFlags = RightHeadedBi; val revFlags = RightHeadedBi; override def stringSuffix = "R" }
  object LeftHeadedIn extends CommonFlags { val flipFlags = LeftHeadedOut; val revFlags = LeftHeadedOut; override def stringPrefix = "-"; override def stringSuffix = "L" }
  object RightHeadedIn extends CommonFlags { val flipFlags = RightHeadedOut; val revFlags = RightHeadedOut; override def stringPrefix = "-"; override def stringSuffix = "R" }
  object LeftHeadedOut extends CommonFlags { val flipFlags = LeftHeadedIn; val revFlags = LeftHeadedIn; override def stringPrefix = "+"; override def stringSuffix = "L" }
  object RightHeadedOut extends CommonFlags { val flipFlags = RightHeadedIn; val revFlags = RightHeadedOut; override def stringPrefix = "+"; override def stringSuffix = "R" }

  def parseBiFlag(v: Int): Flag = v.abs match {
    case 0 => Zero
    case 1 => Bi.DiagLeft
    case 2 => Bi.Orth
    case 3 => Bi.DiagRight
    case 4 => Four
    case 11 => Bi.BlendLeft
    case 13 => Bi.BlendRight
    case 111 => Bi.ExtBlendLeft
    case 113 => Bi.ExtBlendRight
    case 121 => Bi.BlendLeftR1
    case 123 => Bi.BlendRightR1
  }

  def parseInOutFlag(v: Int): Flag = v match {
    case -1 => In.DiagLeft
    case 1 => Out.DiagLeft
    case -2 => In.Orth
    case 2 => Out.Orth
    case -3 => In.DiagRight
    case 3 => Out.DiagRight
    case -11 => In.BlendLeft
    case 11 => Out.BlendLeft
    case -13 => In.BlendRight
    case 13 => Out.BlendRight
    case -111 => In.ExtBlendLeft
    case 111 => Out.ExtBlendLeft
    case -113 => In.ExtBlendRight
    case 113 => Out.ExtBlendRight
    case -121 => In.BlendLeftR1
    case 121 => Out.BlendLeftR1
    case -123 => In.BlendRightR1
    case 123 => Out.BlendRightR1
    case -211 => In.FlexFly01L
    case 211 => Out.FlexFly01L
    case -213 => In.FlexFly01R
    case 213 => Out.FlexFly01R
    case -221 => In.FlexFly12L
    case 221 => Out.FlexFly12L
    case -223 => In.FlexFly12R
    case 223 => Out.FlexFly12R
    case -231 => In.FlexFly34L
    case 231 => Out.FlexFly34L
    case -233 => In.FlexFly34R
    case 233 => Out.FlexFly34R
    case -241 => In.FlexFly45L
    case 241 => Out.FlexFly45L
    case -243 => In.FlexFly45R
    case 243 => Out.FlexFly45R
    case _ => parseBiFlag(v)
  }
}

/** An implementation of `IndexedSeq[Flag]` that is limited to length 4.
  */
class Flags private (fs: Array[Flag], off: Int = 0) extends IndexedSeq[Flag] {

  def apply(i: Int): Flag = {
    require(i >= 0 && i < 4)
    fs((i + off) % 4)
  }
  def length: Int = 4
//  override def stringPrefix: String = "Flags"
  override def toString: String = mkString("(", ",", ")")
  override def canEqual(obj: Any): Boolean = obj.isInstanceOf[Flags]

  def * (rf: RotFlip): Flags = {
    val rotated = if (rf.rot == 0) this else new Flags(fs, (off + 4 - rf.rot) % 4)
    if (rf.flipped)
      Flags(rotated(2).flip, rotated(1).flip, rotated(0).flip, rotated(3).flip)
    else
      rotated
  }

  lazy val symmetries: SymGroup = SymGroup.ofFlags(this)
  def representations: QuotientGroup = symmetries.quotient

  def makeLeftHeaded: Flags = new Flags(fs map (_.makeLeftHeaded), off)
  def makeRightHeaded: Flags = new Flags(fs map (_.makeRightHeaded), off)
}

/** Defines a few common Int tuples for use as Flags.
  */
object Flags {
  type IntFlags = (Int, Int, Int, Int)

  def apply(w: Flag, n: Flag, e: Flag, s: Flag): Flags = new Flags(Array(w, n, e, s))
  def apply(wnes: IndexedSeq[Flag]): Flags = new Flags(wnes.toArray)

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
  val CES = (0,0,1,0)
  val CSW = (0,0,0,1)
  val CWN = (1,0,0,0)
  val CNE = (0,1,0,0)
  val SEC = (0,0,0,-3)
  val WSC = (-3,0,0,0)
  val NWC = (0,-3,0,0)
  val ENC = (0,0,-3,0)
  val CSE = (0,0,0,3)
  val CWS = (3,0,0,0)
  val CNW = (0,3,0,0)
  val CEN = (0,0,3,0)
  // avenue-like diagonals
  val SharedDiagLeft = (-3,1,-3,1)
  val SharedDiagRight = (1,-3,1,-3)
}
