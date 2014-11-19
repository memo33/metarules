package metarules.meta

import org.scalatest.{WordSpec, Matchers}
import Network._

class NetworksSpec extends WordSpec with Matchers {

  "RHW networks" should {
    "have correctly initialized IIDs" in {
      assert {
        values.forall { n =>
          (n.rhwPieceId.isDefined || !n.rhwRangeId.isDefined) &&
            (n.rhwPieceId forall { id => id / 0x10 % 0x10 == n.height }) &&
            (n.rhwRangeId forall { id => id / 0x100000 % 0x10 == n.height && id / 0x10000000 == 5 })
        }
      }
    }
    "coincide with isRhw method" in {
      for (n <- values) {
        n.isRhw should be (RhwNetworks(n))
      }
    }
  }
}
