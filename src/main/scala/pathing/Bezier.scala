package io.github.memo33
package metarules.pathing

import io.github.memo33.scdbpf.DbpfUtil.{Dihedral, RotFlip}

object Bezier {

  type Dec = Float
  type Curve = Dec => Point
  private val Threshold = 1E-8

  case class Point(x: Dec, y: Dec, z: Dec) {
    def * (f: Dec): Point = Point(x * f, y * f, z * f)
    def + (that: Point): Point = Point(x + that.x, y + that.y, z + that.z)
    def - (that: Point): Point = Point(x - that.x, y - that.y, z - that.z)
    infix def dot(that: Point): Dec = x * that.x + y * that.y + z * that.z
    def normSquare: Dec = this dot this
    def norm: Double = Math.sqrt(normSquare)
//    def normalize: Point = {
//      val n = norm
//      require(n > Threshold, "below precision threshold in normalization")
//      this * (1 / n).toFloat
//    }
    def angle(that: Point): Double = {
      Math.acos((this dot that) / this.norm / that.norm)
    }
  }

  implicit object PointIsDihedral extends Dihedral[Point, Dec] {
    def x(p: Point) = p.x
    def y(p: Point) = p.y
    def build(from: Point, x: Dec, y: Dec) = Point(x, y, from.z)
  }

  def cubicCurve(p0: Point, p1: Point, p2: Point, p3: Point): Curve = { (t: Dec) =>
    p0 * ((1-t) * (1-t) * (1-t)) +
    p1 * (3 * t * (1-t) * (1-t)) +
    p2 * (3 * t * t * (1 - t)) +
    p3 * (t * t * t)
  }

  def quadraticCurve(p0: Point, p1: Point, p2: Point): Curve = { (t: Dec) =>
    p0 * ((1-t) * (1-t)) +
    p1 * (2 * t * (1-t)) +
    p2 * (t * t)
  }

  /** Creates a cubic curve from the quadratic control triangle by guessing
    * suitable control points. This function determines the shape of the curve,
    * i.e. its smoothness.
    */
  def curveFromTriangle(p0: Point, p12: Point, p3: Point): Curve = {
    val a = 0.5f // larger values are nearer to p12
    val b = 0.5f // smaller values are nearer to p12
    // TODO consider using the angle as heuristic
    def guess(p0: Point, p12: Point, p3: Point): Point = {
      val dist = (p3 * b + p12 * (1 - b) - p0).norm
      val q = p12 - p0
      p0 + q * (a * dist / q.norm).toFloat
    }
    val p1 = guess(p0, p12, p3)
    val p2 = guess(p3, p12, p0)
    cubicCurve(p0, p1, p2, p3)
//    cubicCurve(p0, p0 * 0.3f + p12 * 0.7f, p3 * 0.3f + p12 * 0.7f, p3)
  }

  def segmentCurve(minDist: Double, maxAngle: Double, curve: Curve): Seq[Point] = {
    val minDistSquare = minDist * minDist
    // technically, this recursive function only checks about every other point
    // for its angle, but it should be good enough
    def segmentRec(p0: Point, t0: Dec, p2: Point, t2: Dec): Seq[Point] = {
      if ((p2 - p0).normSquare < minDistSquare) {
        Seq.empty
      } else {
        val t1 = (t0 + t2) / 2
        val p1 = curve(t1)
        val angle = (p1 - p0).angle(p2 - p1)
        if (angle < maxAngle) {
          Seq(p1)
        } else {
          segmentRec(p0, t0, p1, t1) ++ (p1 +: segmentRec(p1, t1, p2, t2))
        }
      }
    }
    val p0 = curve(0)
    val p2 = curve(1)
    p0 +: segmentRec(p0, 0, p2, 1) :+ p2
  }

  case class Line(p1: Point, p2: Point) {
    def * (rf: RotFlip): Line = Line(p1 *: rf, p2 *: rf)
    infix def intersect(that: Line): Point = {
      val Line(p3, p4) = that
      val denom = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x)
      require(denom.abs > Threshold, "lines must not be parallel")
      val numerX = (p1.x * p2.y - p1.y * p2.x) * (p3.x - p4.x) - (p3.x * p4.y - p3.y * p4.x) * (p1.x - p2.x)
      val numerY = (p1.x * p2.y - p1.y * p2.x) * (p3.y - p4.y) - (p3.x * p4.y - p3.y * p4.x) * (p1.y - p2.y)
      Point(numerX / denom, numerY / denom, p1.z)
    }
    /** Returns 1 if point is on left side of the line, -1 if on the right side, 0 if
      * point is on the line.
      */
    def side(p: Point): Int = {
      ((p2.x - p1.x) * (p.y - p1.y) - (p2.y - p1.y) * (p.x - p1.x)).sign.toInt
    }
    /** Returns true if points are on the same side of this line. Note: Points
      * must not be ''on'' the line.
      */
    def sameSide(p: Point, q: Point): Boolean = {
      val s1 = side(p)
      val s2 = side(q)
      require(s1 != 0 && s2 != 0, "to compare sides, points must not be on the line")
      s1 == s2
    }
  }
}
