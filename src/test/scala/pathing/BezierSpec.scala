package io.github.memo33
package metarules.pathing

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Bezier._

class BezierTest extends AnyWordSpec with Matchers {

  val p0 = Point(2,-6,0)
  val p0b = Point(2,6,0)
  val p2 = Point(-6,2,0)
  val p2b = Point(6,2,0)
  val p1 = Point(2,2,0)
  "Bezier" should {
    "create nice 90 curve" in {
      val curve = curveFromTriangle(p0, p1, p2)
      val minDist = 0.25
      val maxAngle = 0.14 // about 8 degree
      val points = segmentCurve(minDist, maxAngle, curve)
      for (Point(x,y,z) <- points) {
        println(s"$x,$y,$z")
      }
      println("size: " + points.size)
    }
  }

  "Line" should {
    "intersect correctly" in {
      val l1 = Line(p0, p0b)
      val l2 = Line(p2, p2b)
      l1 intersect l2 should be (p1)
    }
  }
}
