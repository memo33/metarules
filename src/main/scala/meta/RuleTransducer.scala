package io.github.memo33
package metarules.meta

import RotFlip._
import group.SymGroup

trait RuleTransducerSyntax { syntax: Syntax =>

import Implicits._

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

  val LOGGER = java.util.logging.Logger.getLogger("networkaddonmod")

  class ResolutionFailed(val tile: SymTile, val rule: Option[Rule[SymTile]], val reason: Throwable, val frame: Option[StackTraceElement]) extends Exception(
    s"ID resolution failed for tile `${tile}`"
    + rule.map { r =>
      val rPadded: String = (0 to 3).find(i => r(i) == tile).map(i => r.toStringUnderlined(i, pad=2)).getOrElse(s"  $r")
      f" of rule%n%n$rPadded%n"
    }.getOrElse("")
    + frame.map(f => f"%nThe rule was generated at ${f.getFileName}:${f.getLineNumber}. Either the rule does not make sense, or an ID for the tile needs to be defined in the resolver.%n").getOrElse("")
    + ResolutionFailed.resolverNamesMessage(reason),
    reason
  )
  object ResolutionFailed {
    def collectResolverNamesFromTrace(reason: Throwable): Seq[String] = {
      Iterator.iterate(reason)(e => Option(e.getCause).filter(_ ne e).orNull)
        .takeWhile(_ != null).take(10)
        .flatMap { e =>
          e.getStackTrace.filter { frame =>
            frame.getClassName.toLowerCase(java.util.Locale.ENGLISH).contains("resolver")
            || frame.getFileName.toLowerCase(java.util.Locale.ENGLISH).contains("resolver")
          }
          .map(_.getClassName)
        }
        .toSeq.reverse.distinct
    }
    def resolverNamesMessage(reason: Throwable): String = {
      val names = collectResolverNamesFromTrace(reason)
      if (names.isEmpty) ""
      else f"""%nResolvers in use: ${names.mkString(", ")}.%n"""
    }
  }

  def apply(rule: Rule[SymTile])(implicit context: Context): Iterator[Rule[IdTile]] = {
    val input = context.preprocess(rule)
    val inputNonEmpty = input.nonEmpty
    val result = input.flatMap(rule => createRules(
      rule.map(tile => try tile.toIdSymTile(context.resolve) catch {
        case scala.util.control.NonFatal(e) => throw new ResolutionFailed(tile, Some(rule), e, frame = None)
      }),
      context.tileOrientationCache.cache,
      context.tileOrientationCache.accum,
    ))
    if (result.isEmpty && inputNonEmpty) {
      LOGGER.warning(s"did not produce any rules for: $rule")
    }
    result.distinct
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
    // Since we do not swap the left and right output tiles in any case, we do
    // not treat the case of equal IDs in a special way anymore.
    // Any duplicates should later be removed by the use of EquivRule.
    ag.flip == 1
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

  /** Some ambiguities are unavoidable, unfortunately, if we want to avoid
    * adding all the symmetries to the RUL2 output (in most situations these
    * would be redundant).
    * See the tests for an explicit example where this is a problem.
    * This function makes an effort to choose the orientations that seem to work
    * best. This takes into account that the game resolves RUL2 overrides from
    * North to South and West to East.
    */
  private[meta] def resolveAmbiguousRhsOrientations(ag: RotFlip, bg: RotFlip, rhs: Iterable[(RotFlip, RotFlip)]): (RotFlip, RotFlip) = {
    // The ambiguities can cause a tile that is normally represented by 0,0 and
    // 1,0 to be represented by 1,0 and 2,0, so we pick the smallest RotFlip that
    // rotates the input tile in the opposite direction.
    // We also make the last tile most significant, as the ambiguity is more commonly on the last tile.
    rhs.minBy { case (cg, dg) => (bg / dg, ag / cg) }
  }

  /** The RHS-orientations contain more possible orientations than the absolute
    * orientations currently stored in repr allows.
    * Therefore, we enlarge the repr by computing the closure of repr under
    * rotations by the relative differences of the RHS-orientations.
    */
  private[meta] def computeExtendedRepr(repr: Set[RotFlip], rhsOrientations: Iterable[RotFlip]): Set[RotFlip] = {
    val g0 = rhsOrientations.head
    val gs = (RotFlip.ValueSet.newBuilder ++= rhsOrientations.tail).result()
    if (gs.isEmpty) {
      // this can happen when the other RHS had the ambiguity, so there is nothing to do
      repr
    } else {
      for (g <- gs; a <- repr; a2 <- Seq(a, a * (g / g0))) yield a2
    }
  }

  private[meta] def accumulateExtendedRepr(
    id: Int,
    repr: Set[RotFlip],
    rhsOrientations: Iterable[RotFlip],
    tileOrientationCache: collection.Map[Int, Set[RotFlip]],
    tileOrientationCacheAccum: collection.mutable.Map[Int, Set[RotFlip]],
  ): Unit = {
    val repr1 = tileOrientationCacheAccum.getOrElse(id, tileOrientationCache.getOrElse(id, repr))
    val repr2 = computeExtendedRepr(repr1, rhsOrientations)
    if (repr2 != repr1) {
      tileOrientationCacheAccum(id) = repr2
    }
  }

  private[meta] def createRules(
    rule: Rule[IdSymTile],
    tileOrientationCache: collection.Map[Int, Set[RotFlip]],
    tileOrientationCacheAccum: collection.mutable.Map[Int, Set[RotFlip]],
  ): Iterator[Rule[IdTile]] = {
    val a = rule(0); val b = rule(1); val c = rule(2); val d = rule(3)
    val aRepr = tileOrientationCache.getOrElse(a.id, a.repr)
    val bRepr = tileOrientationCache.getOrElse(b.id, b.repr)
    val cRepr = tileOrientationCache.getOrElse(c.id, c.repr)
    val dRepr = tileOrientationCache.getOrElse(d.id, d.repr)

    // TODO figure out whether really not to use mapped representations
    require(isReachable(a.symmetries.quotient, a.rf, b.symmetries.quotient, b.rf), "Orientations are not reachable: " + rule.map(t => IdTile(t.id, t.rf)))

    for {
      // TODO possibly, factor needs to be refined in case of equal IIDs
      fac <- if (!a.symmetries.contains(R2F1) || !b.symmetries.contains(R2F1)) Iterator(R0F0, R2F1) else Iterator(R0F0)
      as <- a.symmetries.iterator; ag = a.rf * as * fac
      bs <- b.symmetries.iterator; bg = b.rf * bs * fac
      if !hasSmallerEquivRepr(a.id, ag, b.id, bg)
      if isReachable(aRepr, ag, bRepr, bg)
      rhs = mappedRhs(fac, findRhsOrientation(ag/fac, a.symmetries, aRepr,
                                              bg/fac, b.symmetries, bRepr,
                                              c.rf,   c.symmetries, cRepr,
                                              d.rf,   d.symmetries, dRepr))
      if rhs.nonEmpty //|| {
      //   LOGGER.warning("Could not find RHS orientation:\n" + rule.map(t => IdTile(t.id, t.rf)) +
      //     s"\nfac $fac as $as ag $ag bs $bs bg $bg\n" +
      //     rule.map(t => s"${t.symmetries} ${t.repr}").mkString("\n"))
      //   false
      // }
    } yield {
      if (rhs.size > 1) {
        // In this case, the RHS orientations are ambiguous, which means that
        // the tiles involved on the RHS can appear with non-standard absolute
        // rotations. We compute these additional rotations and add them to the
        // cache, so that other metarules can take the additional rotations into
        // account.
        // As this only affects subsequent metarules, this requires two global
        // runs of the entire metarule compilation process to produce consistent
        // results.
        accumulateExtendedRepr(c.id, cRepr, rhs.map(_._1), tileOrientationCache, tileOrientationCacheAccum)
        accumulateExtendedRepr(d.id, dRepr, rhs.map(_._2), tileOrientationCache, tileOrientationCacheAccum)
      }
      val (cg, dg) = resolveAmbiguousRhsOrientations(ag, bg, rhs)

      val result = Rule(
        IdTile(a.id, ag),
        IdTile(b.id, bg),
        IdTile(c.id, cg),
        IdTile(d.id, dg))

      if (!isReachable(cRepr, cg, dRepr, dg)) {
        LOGGER.warning("unreachable RHS orientation: " + result)
      } else if (!isReachable(aRepr, ag, dRepr, dg)) {
        LOGGER.warning("unreachable orientation (0;3): " + result)
      } else if (!isReachable(cRepr, cg, bRepr, bg)) {
        LOGGER.warning("unreachable orientation (1;2): " + result)
      }

      if (bg.flipped && !bRepr.exists(_.flipped) || !bg.flipped && !bRepr.exists(!_.flipped)) {
        // Usually, tile `a` is always nonflipped in result.
        // If tile `b` does not correspond to valid (absolute) representation, we pick
        // a more intuitive orientation of the rule instead to make the output easier to grasp.
        result.map(_ * R2F1)
      } else {
        result
      }
    }
  }

  val defaultPreprocessor: Rule[SymTile] => Iterator[Rule[SymTile]] = rule => Iterator(rule)

  class TileOrientationCache(
    val cache: collection.mutable.Map[Int, Set[RotFlip]],
    val accum: collection.mutable.Map[Int, Set[RotFlip]],
  )

  case class Context(
    /** The ID resolver. */
    resolve: Tile => IdTile,
    /** A mapping of IDs to non-standard orientations. This is used to overwrite
      * the default orientations that are derived from tile symmetries alone.
      * Sometimes, there is a need for additional non-standard orientations due
      * to some side-effects of other rules. For example, some tiles would be
      * represented by (0,0) and (1,0) only, by default, but in conjunction with
      * some other rules, it can be necessary to represent them by (0,0), (1,0),
      * (2,0) and (3,0). Usually this happens when a rule overwrites a tile with
      * few symmetries by a tile with more symmetries, which can lead to
      * ambiguities.
      * These non-standard orientations can be detected automatically by the
      * rule transducer and are accumulated in `tileOrientationCache.accum`.
      * Add them to the `tileOrientationCache.cache` so that the transducer takes
      * them into account when creating new rules.
      */
    tileOrientationCache: TileOrientationCache = TileOrientationCache(collection.mutable.Map.empty, collection.mutable.Map.empty),
    /** A preprocessor that allows to perform some remapping on the input rules before resolving or transducing. */
    preprocess: Rule[SymTile] => Iterator[Rule[SymTile]] = defaultPreprocessor,
  )
}

} // end of RuleTransducerSyntax
