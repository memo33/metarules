package module

import meta._, Network._, Flags._, Implicits._

trait Adjacencies { this: RhwRuleGenerator =>

  private val NSNS = 0
  private val NSSN = 1
  private val SNNS = 2
  private val SNSN = 3

  private val adjacentNetworksMap: Map[Network, Iterable[(Network, Int)]] = {
    val m = scala.collection.mutable.Map.empty[Network, Iterable[(Network, Int)]]
    import RhwRuleGenerator.HeightLevel
    for (h <- 0 to 2) {
      val cMed = Iterable((h~Rhw6cm, NSNS))
      val cShoulders = Seq(h~Rhw6c, h~Rhw8c, h~Rhw10c)
      for (shoulder <- cShoulders) {
        m(shoulder) = cMed
      }
      val sMed = Iterable((h~Rhw8sm, NSNS))
      val sShoulders = Seq(h~Rhw8s, h~Rhw10s, h~Rhw12s)
      for (shoulder <- sShoulders) {
        m(shoulder) = sMed
      }
      m(h~Rhw6cm) = cShoulders map (n => (n, SNSN))
      m(h~Rhw8sm) = sShoulders map (n => (n, SNSN))
    }
    m.toMap
  }

  private def adjacentNetworks(n: Network): TraversableOnce[(Network, Int)] = adjacentNetworksMap getOrElse (n, {
    if (n == Ave6 || n == Ave8) Iterator((Ave6m, NSNS), (Tla7m, NSNS))
    else if (n == Ave6m || n == Tla7m) Iterator((Ave6, SNSN), (Ave8, SNSN))

    else if (n.typ == AvenueLike || n >= Tla5 && n <= Rd6) Iterator((n, NSSN))

    else if (n == Rhw4) Iterator((Rhw4, NSNS), (Rhw4, NSSN), (Rhw4, SNSN))

    else Iterator.empty
  })

  /** Covers all cases of parallel adjacent +/X-intersections, i.e. OxO, OxD,
    * DxO, DxD, save for 'inner' diagonal intersections.
    */
  def createAdjacentIntersections(main: Network, base: Network, minor: Network): Unit = {
    assert(intersectionAllowed(main, minor))
    val (se, nw) = if (main.typ != AvenueLike) (SE, NW) else (SharedDiagRight, SharedDiagRight) // that way, code below works whether main is avelike or not
    // TODO case of avelike main needs to be tested, e.g. RD4

    for ((adjacent, dirs) <- adjacentNetworks(minor)) {
      val (ns1, nw1, ws1) = if (dirs == NSNS || dirs == NSSN) (NS, NW, WS) else (SN, WN, SW)
      val (ns2, es2, ne2) = if (dirs == NSNS || dirs == SNNS) (NS, ES, NE) else (SN, SE, EN)

      def addRules(adj: Network) = {
        if (intersectionAllowed(base, adj) && intersectionAllowed(main, adj)) {
          Rules += main~WE & minor~ns1    | (base ~> main)~WE & adj~ns2      // OxO
          Rules += main~se~ES & minor~ns1 | (base ~> main)~WN~nw & adj~ns2   // DxO
          if (minor.typ != AvenueLike || dirs == SNNS) {
            Rules += main~WE~EW & minor~nw1 | (base ~> main)~WE~EW & adj~es2   // OxD
            Rules += main~se~ES & minor~ws1 | (base ~> main)~WN~nw & adj~ne2   // DxD
          } else { assert((adj == minor || minor.base.isDefined && minor.base.get == adj) && dirs == NSSN, s"adj $adj minor $minor minbase ${minor.base} dirs $dirs")
            Rules += main~WE~EW & minor~ES              | (base ~> main)~WE~EW & adj~SharedDiagRight   // OxD
            Rules += main~WE~EW & minor~SharedDiagRight | (base ~> main)~WE~EW & adj~WN                // OxD
            Rules += main~se~ES & minor~NE              | (base ~> main)~WN~nw & adj~SharedDiagLeft    // DxD
            Rules += main~se~ES & minor~SharedDiagLeft  | (base ~> main)~WN~nw & adj~SW                // DxD
          }
        }
      }
      addRules(adjacent)
      for (adjBase <- adjacent.base) {
        addRules(adjBase)
      }
    }
    createRules()
  }
}
