package metarules.meta

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import RotFlip._

class EquivRuleSpec extends AnyWordSpec with Matchers {

  "equals and hashcode" should {
    def wrap(id1: Int, rf1: RotFlip, id2: Int, rf2: RotFlip): EquivRule = {
      import internal.IdTile
      new EquivRule(Rule(IdTile(id1, rf1), IdTile(id2, rf2), IdTile(0, R0F0), IdTile(0, R0F0)))
    }
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
    "pass exhaustive test" in {
      val rules = for (a <- RotFlip.values.toSeq; b <- RotFlip.values.toSeq; r <- Seq(wrap(0x100, a, 0x200, b), wrap(0x200, b, 0x100, a))) yield r
      rules.groupBy(identity).values.forall(_.size == 4) shouldBe true
      rules.groupBy(_.hashCode).values.forall(_.size == 4) shouldBe true
      rules.toSet.size shouldBe (2 * 8 * 8 / 4)
    }
    "pass exhaustive test with equal IIDs" in {
      val equivRotations = for (a <- RotFlip.values.toSeq; b <- RotFlip.values.toSeq) yield Seq((a,b), (a*R2F1,b*R2F1), (b*R0F1,a*R0F1), (b*R2F0,a*R2F0))
      // check that equal rules are really equal
      for (rfs <- equivRotations) {
        val rules = rfs.map { case (a,b) => wrapSame(a,b) }
        rules.toSet.size shouldBe 1
        rules.map(_.hashCode).toSet.size shouldBe 1
      }
      // check that unequal rules are really unequal
      val equivRules = equivRotations.map(_.map { case (a,b) => wrapSame(a,b) })
      val m1 = equivRules.groupBy(identity).mapValues(_.size)  // consists of 4s and 2s (weird cases)
      equivRotations.map(_.toSet).distinct.map(_.size) shouldBe equivRules.distinct.map(m1)
      // check that unequal rules do not have hash collisions in this example
      val m2 = equivRules.groupBy(_.hashCode).mapValues(_.size)
      equivRotations.map(_.toSet).distinct.map(_.size) shouldBe equivRules.map(_.hashCode).distinct.map(m2)
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
