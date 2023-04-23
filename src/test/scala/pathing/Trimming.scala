package io.github.memo33
package metarules.pathing

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Trimming._
import Bezier._

class TrimmingSpec extends AnyWordSpec with Matchers {

  "splitAtLine" should {
    "yield expected result" in {
      val ps = Seq(Point(-10,-2,0), Point(-6,2,0))
      val x = Point(-8,0,0)
      splitAtLine(Border.west, ps) should be (Seq(Seq(ps.head, x), Seq(x, ps.last)))
    }
  }

  "trimToTile" should {
    "yield expected result" in {
      val ps = Seq(
        Point(0,-16,0), Point(0,16,0), Point(1,16,0), Point(1,0,0), Point(2,0,0), Point(2,16,0),
        Point(16,16,0), Point(16,6,0), Point(6,6,0), Point(6,0,0), Point(16,0,0))
      val a = Seq(Point(0,-8,0), Point(0,8,0))
      val b = Seq(Point(1,8,0), Point(1,0,0), Point(2,0,0), Point(2,8,0))
      val c = Seq(Point(8,6,0), Point(6,6,0), Point(6,0,0), Point(8,0,0))
      trimToTile(ps) should be (Seq(a, b, c))
    }
    "handle paths ending inside the tile" in {
      val ps = Seq(Point(4,0,0), Point(12,4,0))
      trimToTile(ps) should be (Seq(Seq(Point(4,0,0), Point(8,2,0))))
      trimToTile(ps.reverse) should be (Seq(Seq(Point(8,2,0), Point(4,0,0))))
      val qs = Seq(Point(10,0,0), Point(12,4,0))
      trimToTile(qs) should be (Seq())
      val rs = Seq(Point(8,0,0), Point(12,4,0))
      trimToTile(rs) should be (Seq())  // degenerate case (can arise in stop path construction and might miss some stop points)
    }
  }
}
