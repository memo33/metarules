package metarules.pathing

import org.scalatest.{Matchers, WordSpec}
import Trimming._
import Bezier._

class TrimmingSpec extends WordSpec with Matchers {

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
  }
}
