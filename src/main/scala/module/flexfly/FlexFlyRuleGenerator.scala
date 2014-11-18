package module.flexfly

import meta._, module._, Network._, RotFlip._, Flags._, Implicits._
import FlexFlyTiles._, Adjacencies._, NetworkProperties._

class FlexFlyRuleGenerator(val resolver: IdResolver) extends RuleGenerator {

  def start(): Unit = {
    Seq[IntFlags => IntFlags](identity _, reverseIntFlags _) foreach { orient =>
      // orient is responsible for distinguishing between A1 and A2 curve:
      // we only write code for A1 curve, orient reverses all the flags for us
      for (main <- RhwNetworks from Mis to L4Rhw4) {
        // first establish tiles 2 and 4 of base curve which are not anchors
        Rules += main~orient(T1) | Dirtroad~(0,0,0,0) | % | main~orient(T2)
        Rules += main~orient(T3) | Dirtroad~(0,0,0,0) | % | main~orient(T4)
        Rules += main~orient(T3) * R1F0 | Dirtroad~(0,0,0,0) | % | main~orient(T2) * R1F0
        // connect tile 0 to orthogonal network and tile 6 to diagonal
        Rules += (Dirtroad ~> main)~orient(EW) | main~orient(T0)
        Rules += main~orient(T6) * R3F0 | (Dirtroad ~> main)~orient(NW)
        createRules()

        val deactivated = Rhw12s + L1Rhw12s + L2Rhw12s + Rhw10c + L1Rhw10c + L2Rhw10c
        for (minor <- RhwNetworks if minor.height != main.height && !deactivated(minor); base <- minor.base) {
          val minDirs = { // the directions of minor for which the west edge is a shoulder (possibly empty)
            val b = Seq.newBuilder[IntFlags]
            if (hasRightShoulder(minor)) b += NS
            if (hasLeftShoulder(minor)) b += SN
            b.result
          }
          // crossings of anchor tiles 0, 1, 3 and 6
          for ((t, rot) <- Seq(T0 -> R1F0, T0 -> R3F0, T1 -> R1F0, T1 -> R3F0, T3 -> R3F0, T6 -> R1F0)) {
            Rules += main~orient(t) * rot | minor~WE~EW | main~orient(t) * rot & minor~WE~EW | %   // t < orth
            Rules += main~orient(t) * rot & minor~WE~EW | (base ~> minor)~WE~EW   // t > orth
          }
          for ((t, rot) <- Seq(T3 -> R0F1, T6 -> R0F0); dir <- minDirs) { // same as above but restricted to physically possible directions
            Rules += main~orient(t) * rot | minor~dir * R1F0 | main~orient(t) * rot & minor~dir * R1F0 | %   // t < orth
            Rules += main~orient(t) * rot & minor~dir * R1F0 | base~dir * R1F0 | % | minor~dir * R1F0   // t > orth
          }
          // of non-achor tiles 2 (usual direction) and 4 (alternative direction)
          for (((t, rot), physicalDirs) <- Seq(T2 -> R1F0 -> Seq(NS, SN), T4 -> R0F0 -> minDirs); dir <- physicalDirs) {
            val minSeg = minor~dir * R1F0;     val baseSeg = base~dir * R1F0
            val t3Seg = main~orient(T3) * rot; val tSeg = main~orient(t) * rot
            Rules += t3Seg & minSeg | baseSeg       | %              | tSeg & minSeg   // T3 > t
            Rules += t3Seg          | minSeg        | t3Seg & minSeg | tSeg & minSeg   // T3 < orth
            Rules += t3Seg          | tSeg & minSeg | t3Seg & minSeg | %               // T3 < t
            Rules += tSeg & minSeg  | baseSeg       | %              | minSeg          // t > orth
          }
          // moreover, we need to make sure the override of minor carries over
          // between tiles 3 and 6
          for (dir <- minDirs) {
            Rules += main~orient(T3) & minor~dir * R1F0 | main~orient(T6) | % | main~orient(T6) & minor~dir * R1F0   // T3 > T6
            Rules += main~orient(T3) | main~orient(T6) & minor~dir * R1F0 | main~orient(T3) & minor~dir * R1F0 | %   // T3 < T6
          }

          // Now we still need to connect the end tiles to orth or diag network
          // if crossings are present.
          // First we consider cases in which only one of the two tiles has crossing
          for (minDir <- minDirs) {
            Rules += (Dirtroad ~> main)~orient(EW)                       | main~orient(T0) & minor~minDir
            Rules += (Dirtroad ~> main)~orient(EW) & minor~minDir * R2F0 | main~orient(T0)
            Rules += main~orient(T6) * R3F0                              | (Dirtroad ~> main)~orient(NW) & minor~minDir
            if (!isTripleTile(minor)) { // otherwise physically impossible
              Rules += main~orient(T6) * R3F0      & minor~minDir * R2F0 | (Dirtroad ~> main)~orient(NW)
            }
          }

          // additional crossing of minor and tile 6 in different direction
          Rules += main~orient(T6) * R3F0 & minor~WE~EW | (Dirtroad ~> main)~orient(NW) & (base ~> minor)~WE~EW   // T6 > OxD
          Rules += main~orient(T6) * R3F0 & minor~WE~EW | (Dirtroad ~> main)~orient(NW) & minor~WE~EW             // stability
          Rules += main~orient(T6) * R3F0 & minor~WE~EW | main~orient(NW)               & (base ~> minor)~WE~EW   // stability
          Rules += main~orient(T6) * R3F0 | main~orient(NW) & minor~WE~EW | main~orient(T6) * R3F0 & minor~WE~EW | %   // T6 < OxD

          // Now we consider cases involving two adjacent crossing networks
          for ((other, directions) <- adjacentNetworks(minor)
               if other.isRhw && other != Dirtroad && other.height == minor.height && !deactivated(other)) {
            val (minDir, otherDir) = directions match {
              case NSNS => (NS, NS)
              case NSSN => (NS, SN)
              case SNNS => (SN, NS)
              case SNSN => (SN, SN)
            }
            Rules += (Dirtroad ~> main)~orient(EW) & minor~minDir | main~orient(T0) & other~otherDir
            if (otherDir == NS && hasLeftShoulder(other) || otherDir == SN && hasRightShoulder(other)) { // skip impossible crossings
              Rules += (Dirtroad ~> main)~orient(SE) & minor~minDir | main~orient(T6) * R1F0 & other~otherDir
            }
          }
        }
        createRules()
      }
    }
  }

}
