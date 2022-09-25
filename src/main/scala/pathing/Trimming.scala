package metarules.pathing

import Bezier._
import scdbpf.DbpfUtil.RotFlip._

object Trimming {

  object Border {
    val borders @ Seq(south, west, north, east) = // lower case letters because we cannot assign to stable identifiers this way
      Seq.iterate(Line(Point(-8,-8,0), Point(8,-8,0)), 4)(_ * R1F0)
  }

  /** Splits the points into an alternating sequence of points, each subsequence
    * of points is located on one side of the line. Splitting points ''on'' the
    * line are added at the start and end of each subsequence. The side of, say,
    * the first subsequence of points is undefined. Each subsequence is
    * non-empty (in fact, each subsequence contains at least 2 points).
    */
  private[pathing] def splitAtLine(line: Line, ps: Points): Seq[Points] = {
    ps.foldRight(List.empty[Points]) {
      case (p: Point, Nil) => List(List(p))
      case (p: Point, (q :: qs) :: qss) =>
        if (line.side(p) == 0) {
          List(p) :: (p :: q :: qs) :: qss
        } else if (line.side(q) == 0 && line.side(p) != 0 || line.sameSide(p, q)) {
          (p :: q :: qs) :: qss
        } else {
          val x = line intersect Line(p, q)
          List(p, x) :: (x :: q :: qs) :: qss
        }
      case _ => throw new AssertionError()
    }
  }

  /** Filters the subsequences that are inside of the tile
    */
  private[pathing] def pointsInsideTileAt(border: Line, ps: Points): Seq[Points] = {
    splitAtLine(border, ps).filter(qs => border.side(qs(1)) > 0 || border.side(qs(0)) > 0) // at most one of 0 and 1 might be on the line
  }

  /** Cuts the points to possibly multiple sequences of points that are exactly
    * within the boundaries of the tile.
    */
  def trimToTile(ps: Points): Seq[Points] = {
    Border.borders.foldLeft(Seq(ps)) { (pss, border) =>
      pss.flatMap(qs => pointsInsideTileAt(border, qs))
    }
  }
}
