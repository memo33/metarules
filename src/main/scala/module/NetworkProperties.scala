package metarules.module

import metarules.meta._, Network._

object NetworkProperties {

  val hasRightShoulder: Network => Boolean =
    Set[Network](Rhw6cm, L1Rhw6cm, L2Rhw6cm, Rhw8sm, L1Rhw8sm, L2Rhw8sm, Ave6m, Tla7m).andThen(!_)

  def hasLeftShoulder(n: Network): Boolean = {
    n.typ != AvenueLike &&
    !(n.typ == Symmetrical && !hasRightShoulder(n)) && // this is treated as right shoulder only, for efficiency
    !(n >= Rhw8s && n <= L2Rhw10c) &&
    !(n >= Tla5 && n <= Ave6m)
  }

  def isDoubleTile(n: Network): Boolean = {
    n.typ == AvenueLike || n >= Rhw8sm && n <= L2Rhw12s || n >= Tla5 && n <= Rd6
  }
  def isTripleTile(n: Network): Boolean = {
    n >= Rhw6cm && n <= L2Rhw10c || n >= Ave6 && n <= Ave6m
  }
  def isSingleTile(n: Network): Boolean = !isDoubleTile(n) && !isTripleTile(n)

}
