package metarules.meta

import RotFlip._
import Group.SymGroup
import Implicits._
import Network._
import Tile.{projectLeft, projectRight}

/** The main logic to translate meta code (`Rule[Tile]`) to proper RUL2 code
  * (`Rule[IdTile]`). The crux of that is that a single meta rule can result in
  * multiple RUL2 rules, based on the symmetries of the tiles involved. Exactly
  * those rules and orientations will be generated that are necessary. Compare,
  * for example:
  *
  * {{{
  * scala> transduce( L1Rhw2~WE | (Dirtroad ~> L1Rhw2)~WE )
  * Rule( 0x57100000,1,0 | 0x57000000,1,0 | 0x57100000,1,0 | 0x57100000,1,0 )
  * Rule( 0x57100000,3,0 | 0x57000000,3,0 | 0x57100000,3,0 | 0x57100000,3,0 )
  *
  * scala> transduce( L1Rhw3~WE | (Dirtroad ~> L1Rhw3)~WE )
  * Rule( 0x57110000,3,0 | 0x57000000,1,0 | 0x57110000,3,0 | 0x57110000,3,0 )
  * Rule( 0x57110000,3,0 | 0x57000000,3,0 | 0x57110000,3,0 | 0x57110000,3,0 )
  * Rule( 0x57110000,1,0 | 0x57000000,3,0 | 0x57110000,1,0 | 0x57110000,1,0 )
  * Rule( 0x57110000,1,0 | 0x57000000,1,0 | 0x57110000,1,0 | 0x57110000,1,0 )
  *
  * scala> transduce( Mis~WE & Rail~NW | (Dirtroad ~> Mis)~WE & Dirtroad~ES )
  * Rule( 0x57024580,1,0 | 0x57004A00,3,0 | 0x57024580,1,0 | 0x57024A00,3,0 )
  * }}}
  *
  * If in doubt, start up the `sbt console` and use the `transduce` function to
  * check the output.
  */
object RuleTransducer {

  private[meta] def tileToIdSymTile(t: Tile)(implicit resolve: IdResolver): IdSymTile =
    new IdSymTile(t, resolve(t))

  def apply(rule: Rule[Tile])(implicit resolve: IdResolver): TraversableOnce[Rule[IdTile]] = {
    multiplyTla(rule).flatMap(rule => createRules(rule map tileToIdSymTile))
  }

  /** Tests whether the orientation of the two tiles can occur according to
    * their quotients.
    */
  private[meta] def isReachable(aRepr: Set[RotFlip], ag: GroupElement, bRepr: Set[RotFlip], bg: GroupElement) = {
//    bRepr.contains(bg / ag) ||
//    aRepr.contains(ag / bg)
    possibleMapOrientation(aRepr, ag, bRepr, bg).nonEmpty
  }

  /** Tests if the LHS has a 'smaller' representation (for some fixed metric)
    * that is equivalent.
    */
  private[meta] def hasSmallerEquivRepr(aId: Int, ag: GroupElement, bId: Int, bg: GroupElement) = {
    ag.flip == 1 || aId == bId && bg.flip == 0 && ag.rot > (bg * R2F0).rot
  }

  /** Determines possible orientation of the world map according to the
    * quotients of the tiles.
    */
  private[meta] def possibleMapOrientation(aRepr: Set[RotFlip], ag: GroupElement, bRepr: Set[RotFlip], bg: GroupElement): Set[RotFlip] = {
    RotFlip.values.filter(rf => (ag / rf) ∈ aRepr && (bg / rf) ∈ bRepr)
  }

  /** Determines the best orientation for RHS tiles.
    */
  private[meta] def findRhsOrientation(
      ag: GroupElement, asg: SymGroup, aRepr: Set[RotFlip],
      bg: GroupElement, bsg: SymGroup, bRepr: Set[RotFlip],
      crf: GroupElement, csg: SymGroup, cRepr: Set[RotFlip],
      drf: GroupElement, dsg: SymGroup, dRepr: Set[RotFlip]): Set[(GroupElement, GroupElement)] = {

    for {
      m <- possibleMapOrientation(aRepr, ag, bRepr, bg)
      cg <- RotFlip.values if (R0F0 / crf * cg) ∈ csg && (cg / m) ∈ cRepr
      dg <- RotFlip.values if (R0F0 / drf * dg) ∈ dsg && (dg / m) ∈ dRepr
    } yield (cg, dg)
  }

  /** Separating this function helps overcome implicit expansion problems. */
  private[meta] def mappedRhs(fac: RotFlip, rhs: Iterable[(RotFlip, RotFlip)]): Iterable[(RotFlip, RotFlip)] = {
    rhs map { case (cg, dg) => (cg * fac, dg * fac) }
  }

  private[meta] def createRules(rule: Rule[IdSymTile]): TraversableOnce[Rule[IdTile]] = {
    val a = rule(0); val b = rule(1); val c = rule(2); val d = rule(3)
    // TODO figure out whether really not to use mapped representations
    require(isReachable(a.symmetries.quotient, a.rf, b.symmetries.quotient, b.rf), "Orientations are not reachable: " + rule.map(t => IdTile(t.id, t.rf)))

    val result = for {
      // TODO possibly, factor needs to be refined in case of equal IIDs
      fac <- if (!a.symmetries.contains(R2F1) || !b.symmetries.contains(R2F1)) Seq(R0F0, R2F1) else Seq(R0F0)
      as <- a.symmetries; ag = a.rf * as * fac
      bs <- b.symmetries; bg = b.rf * bs * fac
      if !hasSmallerEquivRepr(a.id, ag, b.id, bg)
      if isReachable(a.repr, ag, b.repr, bg)
      rhs = mappedRhs(fac, findRhsOrientation(ag/fac, a.symmetries, a.repr,
                                              bg/fac, b.symmetries, b.repr,
                                              c.rf,   c.symmetries, c.repr,
                                              d.rf,   d.symmetries, d.repr))
      if rhs.nonEmpty //|| {
//        println("Warning: Could not find RHS orientation:\n" + rule.map(t => IdTile(t.id, t.rf)) +
//          s"\nfac $fac as $as ag $ag bs $bs bg $bg\n" +
//          rule.map(t => s"${t.symmetries} ${t.repr}").mkString("\n"))
//        false
//      }
    } yield {
      if (rhs.size > 1) {
        "Warning: ambiguous RHS orientations: " + rhs
      }
      val (cg, dg) = rhs.head

      val result = Rule(
        IdTile(a.id, ag),
        IdTile(b.id, bg),
        IdTile(c.id, cg),
        IdTile(d.id, dg))

      if (!isReachable(c.repr, cg, d.repr, dg)) {
        println("Warning: unreachable RHS orientation: " + result)
      } else if (!isReachable(a.repr, ag, d.repr, dg)) {
        println("Warning: unreachable orientation (0;3): " + result)
      } else if (!isReachable(c.repr, cg, b.repr, bg)) {
        println("Warning: unreachable orientation (1;2): " + result)
      }

      result
    }
    if (result.isEmpty) {
      println("Warning: did not produce any rules for: " + rule.map(t => IdTile(t.id, t.rf)))
    }
    result
  }

  private[meta] def multiplyTla(rule: Rule[Tile]): TraversableOnce[Rule[Tile]] = {
    if (!rule.exists(_.segs.exists(_.network.isTla))) {
      Iterator(rule)
    } else if (rule.forall(t => t.segs.forall(!_.network.isTla) ||
        !t.symmetries.quotient.exists(_.flipped))) {
      Iterator(rule map projectLeft)
    } else {
      Iterator(rule map projectLeft, rule map projectRight)
    }
  }
}
