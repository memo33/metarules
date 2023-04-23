package io.github.memo33
package metarules.meta

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import RotFlip._
import group.SymGroup
import group.SymGroup._
import internal.DummyNetwork._, Implicits._
import Flags._

class RuleTransduceSpec2 extends AnyWordSpec with Matchers {

  val resolver = {
    val map = scala.collection.mutable.Map.empty[Tile, IdTile]
    def add(tile: Tile, id: Int): Unit = {
      assert(!map.contains(tile))
      for (rf <- RotFlip.values) {
        val idTile = IdTile(id, rf)
        map.getOrElseUpdate(tile * rf, idTile)
      }
    }
    add(L2Rhw2~NS & Rhw4~EW, 0x57201D00)
    add(Dirtroad~EW & L1Rhw2~NS, 0x57101A00)
    add(L2Rhw2~NS & L1Rhw2~EW, 0x57201A10)
    add(L2Rhw2~NS, 0x57200000)
    add(Dirtroad~NS, 0x57000000)
    add(Mis~NS, 0x57020000)
    add(Ave6m~ES & Ave6m~WS, 0x51219E8E)
    add(Ave6m~ES & Road~WS, 0x51218180)
    add(Road~ES & Road~SW, 0x00000700)
    add(Road~NS, 0x00004B00)
    add(L1Rhw2~NS, 0x57100000)
    add(Dirtroad~NS & Dirtroad~EW, 0x57001A00)
    add(L1Rhw2~WE & L1Rhw2~NS, 0x57101A10)
    map.toMap
  }

  "RuleTransducer" should {
    "determine unique smallest representation" in {
      for (ag <- RotFlip.values; bg <- RotFlip.values) {
        val aId = 1
        val bId = 2
        val equivalentOrientations = Seq(
          ((aId, ag), (bId, bg)),
          // ((bId, bg * R2F0), (aId, ag * R2F0)),  // would fail, since we do not swap tiles in the output
          // ((bId, bg * R0F1), (aId, ag * R0F1)),
          ((aId, ag * R2F1), (bId, bg * R2F1)))
        equivalentOrientations.filter(r => !RuleTransducer.hasSmallerEquivRepr(r._1._1, r._1._2, r._2._1, r._2._2)).toSet should have size 1
      }
    }

    "determine unique smallest representation for equal IIDs" in {
      for (ag <- RotFlip.values; bg <-RotFlip.values) {
        val aId = 1
        val equivalentOrientations1 = Seq(  // equal IDs
          ((aId, ag), (aId, bg)),
          // ((aId, bg * R2F0), (aId, ag * R2F0)),  // would fail, since we do not swap tiles in the output
          // ((aId, bg * R0F1), (aId, ag * R0F1)),
          ((aId, ag * R2F1), (aId, bg * R2F1)))
        equivalentOrientations1.filter(r => !RuleTransducer.hasSmallerEquivRepr(r._1._1, r._1._2, r._2._1, r._2._2)).toSet should have size 1

        val context = RuleTransducer.Context(resolver)
        RuleTransducer(
          IdTile(1, ag, noSymmetries) | IdTile(1, bg, noSymmetries) | IdTile(2, ag, noSymmetries) | IdTile(2, bg, noSymmetries)
        )(context).toSeq shouldBe Symbol("nonEmpty")
      }
    }

    "detect ambiguities and take them into account in subsequent metarules" in {
      val context = RuleTransducer.Context(resolver, collection.mutable.Map.empty[Int, Set[RotFlip]])
      /* Initially, the possible absolute rotations of this tile are just these two.
       */
      (L2Rhw2~EW & L1Rhw2~NS).toIdSymTile(resolver).repr shouldBe (R0F0+R1F0)
      /* That is why the rule transducer only produces these two lines of RUL2.
       */
      RuleTransducer(L2Rhw2~EW & L1Rhw2~NS | (Dirtroad~>L2Rhw2)~EW)(context).toSeq shouldBe Seq(
        Rule(0x57201A10,1,0,0x57000000,1,0,0x57201A10,1,0,0x57200000,1,0),
        Rule(0x57201A10,3,0,0x57000000,3,0,0x57201A10,3,0,0x57200000,3,0))
      /* In the following configuration, the tile can end up with an absolute
       * rotation of R2F0. Since RUL2 code is based on relative rotations that
       * is automatically applied in various orientations of the world map, this
       * ambiguity cannot be avoided.
       */
      RuleTransducer(L2Rhw2~EW & Rhw4~NS | (Dirtroad~>L2Rhw2)~EW & L1Rhw2~NS)(context).toSeq shouldBe Seq(
        // (The following comments are outdated and not relevant anymore after the introduction of tileOrientationCache)
        //   fails in world map NS direction
        //     Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        //     Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0)
        //   fails in world map SN direction
        //     Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        //     Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0)
        //   fails both in NS and SN direction
        //     Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        //     Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0)
        //   This one mostly works with just a tiny instability in EW direction (apart
        //   from the instability, this works not because the orientations are always
        //   correct, but in the case they are wrong, the game evaluates the overrides in
        //   a suitable order in the world map direction West to East).
        Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0))
      /* Therefore, we store the information that the tile can have additional rotations.
       */
      context.tileOrientationCache shouldBe Map(0x57201A10 -> (R0F0+R1F0+R2F0+R3F0))
      /* And subsequently we take the additional rotations into account in all other metarules,
       * so the following metarule now produces four lines of RUL2 (instead of two).
       */
      RuleTransducer(L2Rhw2~EW & L1Rhw2~NS | (Dirtroad~>L2Rhw2)~EW)(context).toSeq shouldBe Seq(
        Rule(0x57201A10,1,0,0x57000000,1,0,0x57201A10,1,0,0x57200000,1,0),
        Rule(0x57201A10,1,0,0x57000000,3,0,0x57201A10,1,0,0x57200000,3,0),
        Rule(0x57201A10,3,0,0x57000000,1,0,0x57201A10,3,0,0x57200000,1,0),
        Rule(0x57201A10,3,0,0x57000000,3,0,0x57201A10,3,0,0x57200000,3,0))
      /* The second run should not change the cached orientations anymore.
       */
      context.tileOrientationCache shouldBe Map(0x57201A10 -> (R0F0+R1F0+R2F0+R3F0))
      RuleTransducer(L2Rhw2~EW & Rhw4~NS | (Dirtroad~>L2Rhw2)~EW & L1Rhw2~NS)(context).toSeq shouldBe Seq(
        Rule(0x57201D00,3,0,0x57101A00,0,0,0x57201D00,3,0,0x57201A10,3,0),
        Rule(0x57201D00,3,0,0x57101A00,2,0,0x57201D00,3,0,0x57201A10,1,0))
      context.tileOrientationCache shouldBe Map(0x57201A10 -> (R0F0+R1F0+R2F0+R3F0))
    }

    "detect ambiguities of tiles involving mirrorings" in {
      val context = RuleTransducer.Context(resolver, collection.mutable.Map.empty[Int, Set[RotFlip]])
      (Ave6m~ES & Ave6m~NE).toIdSymTile(resolver).repr shouldBe (R0F0+R1F0+R2F0+R3F0)
      /* Originally, we just get one line of RUL2.
       */
      RuleTransducer(Ave6m~ES & Ave6m~NE | (Road~>Ave6m)~NW & (Road~>Ave6m)~WS)(context).toSeq shouldBe Seq(
        Rule(0x51219E8E,3,0,0x00000700,1,0,0x51219E8E,3,0,0x51219E8E,1,0))
      (Ave6m~ES & Ave6m~NE).toIdSymTile(resolver).repr shouldBe (R0F0+R1F0+R2F0+R3F0)
      /* But here we notice that the tile can end up flipped.
       */
      RuleTransducer(Ave6m~ES & (Road~>Ave6m)~NE | (Road~>Ave6m)~NW & Ave6m~WS)(context).toSeq shouldBe Seq(
        Rule(0x51218180,3,0,0x51218180,3,1,0x51219E8E,1,1,0x51219E8E,3,1))
      context.tileOrientationCache shouldBe Map(0x51219E8E -> (R0F0+R1F0+R2F0+R3F0+R0F1+R3F1+R2F1+R1F1))
      /* Hence subsequently the possibility of flipped absolute rotations
       * results in more lines of RUL2.
       */
      RuleTransducer(Ave6m~ES & Ave6m~NE | (Road~>Ave6m)~NW & (Road~>Ave6m)~WS)(context).toSeq shouldBe Seq(
        Rule(0x51219E8E,3,0,0x00000700,1,0,0x51219E8E,3,0,0x51219E8E,1,0),
        Rule(0x51219E8E,1,1,0x00000700,1,0,0x51219E8E,1,1,0x51219E8E,1,0))  // here, 2nd tile is always non-flipped
    }
  }

  "isReachable" should {
    "work properly" in {
      import RuleTransducer.isReachable
      isReachable(Dih2A.quotient, R0F0, Dih2A.quotient, R0F0) should be (true)
      isReachable(Dih2A.quotient, R0F0, Dih2A.quotient, R2F0) should be (false)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R0F0) should be (true)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R1F0) should be (true)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R2F0) should be (true)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R3F0) should be (true)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R0F0) should be (false)
      isReachable(Dih2A.quotient, R3F0, Dih4.quotient, R1F0) should be (false)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R2F0) should be (false)
      isReachable(Dih2A.quotient, R1F0, Dih4.quotient, R3F0) should be (false)
    }
  }

  "hasSmallerEquivRepr" should {
    val context = RuleTransducer.Context(resolver)
    "work for tiles with same IID" in {
      import RuleTransducer.hasSmallerEquivRepr
      def count(ag: GroupElement, bg: GroupElement): Unit = {
        val l1 = Seq(ag, ag * R2F1, bg * R0F1, bg * R2F0)
        val l2 = Seq(bg, bg * R2F1, ag * R0F1, ag * R2F0)
        withClue(s"$ag $bg:") {
          (l1 zip l2).toSet map { tup: Tuple2[GroupElement, GroupElement] =>
            !hasSmallerEquivRepr(42, tup._1, 42, tup._2)
          } count (_ == true) should be (1)
        }
      }
      val vs = Seq(R0F0, R1F0, R2F0, R3F0, R0F1, R1F1, R2F1, R3F1)
      for (ag <- vs; bg <- vs) {
        count(ag, bg)
      }
    }
    "be correct for tiles with same IID" in {
      RuleTransducer(Road~NS | Road~>Road~WE)(context).toSeq.size should be (RuleTransducer(L1Rhw2~NS | Road~>L1Rhw2~WE)(context).toSeq.size)
    }
  }

  "possibleMapOrientation" should {
    "pass test cases" in {
      import RuleTransducer.possibleMapOrientation
      possibleMapOrientation(Dih4.quotient, R0F0, Dih4.quotient, R0F0) should be (Set(R0F0))
      possibleMapOrientation(Dih4.quotient, R1F0, Dih4.quotient, R1F0) should be (Set(R1F0))
      possibleMapOrientation(Dih4.quotient, R0F1, Dih4.quotient, R0F1) should be (Set(R0F1))

      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R0F0) should be (Set(R0F0))
      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R1F0) should be (Set(R0F0))
      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R2F0) should be (Symbol("empty"))
      possibleMapOrientation(Dih4.quotient, R0F0, Dih2A.quotient, R0F1) should be (Symbol("empty"))

      possibleMapOrientation(Dih2A.quotient, R0F0, Dih2A.quotient, R0F0) should be (Set(R0F0, R3F0))
      possibleMapOrientation(Dih2A.quotient, R1F0, Dih2A.quotient, R1F0) should be (Set(R0F0, R1F0))
      possibleMapOrientation(Dih2A.quotient, R0F0, Dih2A.quotient, R1F0) should be (Set(R0F0))
      possibleMapOrientation(Dih2A.quotient, R1F0, Dih2A.quotient, R0F0) should be (Set(R0F0))

      possibleMapOrientation(Dih2A.quotient, R0F0, Cyc2B.quotient, R0F0) should be (Set(R0F0, R3F0))
      possibleMapOrientation(Dih2A.quotient, R0F0, Cyc2B.quotient, R1F0) should be (Set(R0F0, R3F0))

      possibleMapOrientation(Cyc2B.quotient, R0F0, Cyc2B.quotient, R2F0) should be (Set(R0F0, R1F0, R2F0, R3F0))
      possibleMapOrientation(Cyc2B.quotient, R0F0, Cyc2B.quotient, R1F0) should be (Set(R0F0, R1F0, R2F0, R3F0))

      for (sg <- SymGroup.values; h <- RotFlip.values) {
        possibleMapOrientation(sg.quotient, R0F0, Cyc1.quotient, h) should be (sg.quotient.map((rf: RotFlip) => R0F0 / rf))
      }
    }
  }

  "possibleMapOrientation" should {
    import RuleTransducer._
    import group.Quotient
    "coincide with isReachable" in {
      for (aRepr <- Quotient.values; bRepr <- Quotient.values;
           ag <- RotFlip.values; bg <- RotFlip.values if isReachable(aRepr, ag, bRepr, bg)) {
        possibleMapOrientation(aRepr, ag, bRepr, bg) should not be (Symbol("empty"))
      }
      val aRepr = Set(R0F1, R1F1, R2F1, R3F1); val bRepr = Quotient.Cyc2A
      isReachable(aRepr, R1F0, bRepr, R1F0) should be (false)
      possibleMapOrientation(aRepr, R1F0, bRepr, R1F0) should be (Symbol("empty"))
    }
  }

  class DummyIdSymTile(override val id: Int, override val rf: RotFlip, override val symmetries: SymGroup) extends IdSymTile(null, IdTile(0, null))
  def DIST(id: Int, rf: RotFlip, sg: SymGroup) = new DummyIdSymTile(id, rf, sg)

  "RuleTransducer" should {
    import RuleTransducer.createRules
    implicit val context = RuleTransducer.Context(resolver)
    val c = DIST(0x3000, R0F0, Cyc1)
    val d = DIST(0x4000, R0F0, Cyc1)
    val s = Seq(
      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Dih2A), 2),
      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R1F0, Dih2A), 4),
      (DIST(0x1000, R0F0, Dih2A), DIST(0x1000, R0F0, Dih2A), 2),  // debatable since ambiguous

      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Cyc1), 4),

      (DIST(0x1000, R0F0, Cyc1),  DIST(0x2000, R0F0, Cyc1), 1),
      (DIST(0x1000, R0F0, Cyc1),  DIST(0x1000, R0F0, Cyc1), 1),

      (DIST(0x1000, R0F0, Cyc2B), DIST(0x2000, R0F0, Cyc2B), 2),
      (DIST(0x1000, R0F0, Cyc2B), DIST(0x1000, R0F0, Cyc2B), 2),
      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Cyc2B), 4),

      (DIST(0x1000, R0F0, Dih2A), DIST(0x2000, R0F0, Dih4), 4),
      (DIST(0x1000, R0F0, Dih4),  DIST(0x2000, R0F0, Dih4), 4)
    )

    "generate correct number of rules" in {
      for ((a, b, x) <- s) {
        val rules = createRules(Rule(a,b,c,d), context.tileOrientationCache).to(Iterable)
        withClue(s"${a.symmetries} ${b.symmetries}\n${rules.mkString("\n")}") {
          rules should have size (x)
        }
      }
    }

    "produce proper output orientation for orth asymm overrides" in {
      val rule = Mis~EW | Dirtroad~EW | Mis~EW | Mis~EW
      rule(0).symmetries should be (Cyc2B)
      rule(1).symmetries should be (Dih2A)
      rule(3).symmetries should be (Cyc2B)
      for (r <- RuleTransducer(rule)) withClue(r) {
        r(2).rf should be (r(0).rf)
        r(3).rf should be (r(0).rf)
      }
    }

    "produce proper output orientation for orth symm overrides" in {
      val rule = L2Rhw2~WE | Dirtroad~WE & L1Rhw2~NS | L2Rhw2~WE | L2Rhw2~WE & L1Rhw2~NS
      rule foreach (_.symmetries should be (Dih2A))
      for (r <- RuleTransducer(rule)) withClue(r) {
        r(2).rf should be (r(0).rf)
        r(3).rf should be (r(0).rf)
      }
    }

    def parseRules(text: String): Seq[Rule[IdTile]] = {
      text.linesIterator.map(_.split(";", 2)(0).trim).filterNot(_.isEmpty).map[Rule[IdTile]] { line =>
        val ts = line.split(",|=").grouped(3).toSeq.map(tup =>
          IdTile(java.lang.Long.decode(tup(0)).toInt, RotFlip(tup(1).toInt, tup(2).toInt)))
        Rule(ts(0), ts(1), ts(2), ts(3))
      }
    }.toSeq

    def compareRules(a: Seq[Rule[IdTile]], b: Seq[Rule[IdTile]]): Unit = {
      a should have size b.size
      val sa = (a map (_.toString)).sorted
      val sb = (b map (_.toString)).sorted
      for ((r1, r2) <- sa zip sb) withClue(r1 + " <- auto generated\n" + r2 + " <- desired\n") {
        r1 should be (r2)
      }
    }

    "produce proper output for symm +intersections" in {
      val rules = Seq(
        L1Rhw2~WE               | Dirtroad~WE & Dirtroad~NS | L1Rhw2~WE               | L1Rhw2~WE & Dirtroad~NS,
        L1Rhw2~WE & Dirtroad~NS | Dirtroad~WE               | L1Rhw2~WE & Dirtroad~NS | L1Rhw2~WE,
        L1Rhw2~WE               | Dirtroad~WE & L1Rhw2~NS   | L1Rhw2~WE               | L1Rhw2~WE & L1Rhw2~NS,
        L1Rhw2~WE & L1Rhw2~NS   | Dirtroad~WE               | L1Rhw2~WE & L1Rhw2~NS   | L1Rhw2~WE )

      // to be sure, check symmetries first
      rules(0)(0).symmetries should be (Dih2A)
      rules(0)(1).symmetries should be (Dih4)
      rules(0)(3).symmetries should be (Dih2A)
      rules(1)(1).symmetries should be (Dih2A)
      rules(2)(1).symmetries should be (Dih2A)
      rules(2)(3).symmetries should be (Dih4)

      val autoRules = rules.flatMap(RuleTransducer(_)).toSeq

      val matchRules = parseRules("""
        ;single in
        0x57100000,1,0,0x57001A00,0,0=0x57100000,1,0,0x57101A00,1,0
        0x57100000,1,0,0x57001A00,1,0=0x57100000,1,0,0x57101A00,1,0
        0x57100000,3,0,0x57001A00,2,0=0x57100000,3,0,0x57101A00,3,0
        0x57100000,3,0,0x57001A00,3,0=0x57100000,3,0,0x57101A00,3,0
        ;single out
        0x57101A00,1,0,0x57000000,1,0=0x57101A00,1,0,0x57100000,1,0
        0x57101A00,3,0,0x57000000,3,0=0x57101A00,3,0,0x57100000,3,0

        ;double in
        0x57100000,1,0,0x57101A00,2,0=0x57100000,1,0,0x57101A10,1,0
        0x57100000,3,0,0x57101A00,0,0=0x57100000,3,0,0x57101A10,3,0
        ;double out
        0x57101A10,1,0,0x57000000,1,0=0x57101A10,1,0,0x57100000,1,0
        0x57101A10,3,0,0x57000000,3,0=0x57101A10,3,0,0x57100000,3,0
        ;double stability
        0x57101A10,0,0,0x57000000,1,0=0x57101A10,0,0,0x57100000,1,0
        0x57101A10,2,0,0x57000000,3,0=0x57101A10,2,0,0x57100000,3,0
        0x57100000,1,0,0x57101A00,0,0=0x57100000,1,0,0x57101A10,0,0
        0x57100000,3,0,0x57101A00,2,0=0x57100000,3,0,0x57101A10,2,0
      """)
      compareRules(autoRules, matchRules)
    }
  }
}
