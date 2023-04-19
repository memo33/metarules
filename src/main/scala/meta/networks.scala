package io.github.memo33
package metarules.meta

sealed abstract class NetworkType
case object Symmetrical extends NetworkType
case object Asymmetrical extends NetworkType
case object AvenueLike extends NetworkType

package internal {

  /** Only for testing purposes and REPL. */
  final class DummyNetwork private (val typ: NetworkType) extends DummyNetwork.AbstractNetwork
  object DummyNetwork extends Syntax {
    type Network = DummyNetwork
    val Road, Rail, Street, Lightrail, Dirtroad, L1Rhw2, L2Rhw2, Ave6m = new DummyNetwork(Symmetrical)
    val Avenue = new DummyNetwork(AvenueLike)
    val Mis, Rhw4, Rhw6s = new DummyNetwork(Asymmetrical)
  }

}
