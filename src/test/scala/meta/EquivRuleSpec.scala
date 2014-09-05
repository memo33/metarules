package meta

import org.scalatest.{WordSpec, Matchers}
import RotFlip._

class EquivRuleSpec extends WordSpec with Matchers {

  "equals and hashcode" should {
    def wrap(id1: Int, rf1: RotFlip, id2: Int, rf2: RotFlip): EquivRule =
      new EquivRule(Rule(IdTile(id1, rf1), IdTile(id2, rf2), IdTile(0, R0F0), IdTile(0, R0F0)))
    def wrapSame(rf1: RotFlip, rf2: RotFlip): EquivRule = wrap(0x100, rf1, 0x100, rf2)

    def test(er1: EquivRule, er2: EquivRule): Unit = {
      er1 should be (er2)
      er1.hashCode should be (er2.hashCode)
    }
    def testNot(er1: EquivRule, er2: EquivRule): Unit = {
      er1 should not be (er2)
      er1.hashCode should not be (er2.hashCode)
    }

    "fulfill their contracts" in {
      test(wrapSame(R0F0, R0F0), wrapSame(R0F0, R0F0))
      test(wrapSame(R0F0, R0F0), wrapSame(R2F1, R2F1))
      test(wrapSame(R2F0, R2F0), wrapSame(R0F1, R0F1))
      test(wrapSame(R2F0, R2F0), wrapSame(R2F1, R2F1))
      test(wrapSame(R2F1, R2F1), wrapSame(R0F1, R0F1))

      test(wrapSame(R0F0, R1F0), wrapSame(R3F0, R2F0))
      test(wrapSame(R1F0, R0F0), wrapSame(R2F0, R3F0))
      test(wrapSame(R0F0, R1F0), wrapSame(R2F1, R3F1))
      test(wrapSame(R0F0, R3F0), wrapSame(R1F0, R2F0))
      test(wrapSame(R3F0, R0F0), wrapSame(R2F0, R1F0))
      test(wrapSame(R0F1, R3F0), wrapSame(R1F0, R2F1))
      test(wrapSame(R3F0, R0F1), wrapSame(R2F1, R1F0))

      testNot(wrapSame(R0F0, R1F0), (wrapSame(R2F0, R3F0)))
      test(wrapSame(R3F0, R3F1), wrapSame(R1F1, R1F0))
    }
    "really fulfill their contracts" in { // uhoh, some weird cases here
      test   (wrap(0x100, R3F0, 0x200, R3F1), wrap(0x100, R1F1, 0x200, R1F0))
      test   (wrap(0x100, R3F0, 0x200, R3F1), wrap(0x200, R1F1, 0x100, R1F0))
      testNot(wrap(0x100, R0F0, 0x200, R2F0), wrap(0x100, R2F0, 0x200, R0F0))
      testNot(wrap(0x100, R0F0, 0x200, R2F0), wrap(0x200, R2F0, 0x100, R0F0))
      test   (wrap(0x100, R0F0, 0x200, R2F0), wrap(0x200, R2F1, 0x100, R0F1))
      test   (wrap(0x100, R0F0, 0x200, R2F0), wrap(0x100, R2F1, 0x200, R0F1))
      test   (wrap(0x100, R0F0, 0x200, R2F0), wrap(0x200, R0F0, 0x100, R2F0))

      test   (wrap(0x100, R3F0, 0x100, R3F1), wrap(0x100, R1F1, 0x100, R1F0))
      testNot(wrap(0x100, R0F0, 0x100, R2F0), wrap(0x100, R2F0, 0x100, R0F0))
      test   (wrap(0x100, R0F0, 0x100, R2F0), wrap(0x100, R2F1, 0x100, R0F1))
    }
  }
}

/* A snippet for testing different primes for collisions of EquivRule.hashCode
 */
//def duplicates(prime: Int): Iterable[Set[Rule[IdTile]]] = {
//  val collisions = scala.collection.mutable.HashMap.empty[Int, Set[EquivRule]]
//  rules.foreach { r => val w = new EquivRule(r,prime); val s = collisions.getOrElse(w.hashCode, Set.empty); collisions(w.hashCode) = s + w }
//  collisions.filter{ case (_, s) => s.size > 1 }.values.map(_.map(_.rule))
//}
