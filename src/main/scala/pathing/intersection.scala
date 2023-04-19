package metarules.pathing

import scala.collection._
import scala.collection.immutable.Seq
import Bezier._
import io.github.memo33.scdbpf.Sc4Path, Sc4Path.{TransportType => TT, _}, Cardinal._

abstract class Connection
/** Curved connection in the following format:
  * from  – the from-direction
  * start - the line that determines the start of the curve if crossed with from
  * to    - the to-direction
  * end   - the line that deterimens the end point of the curve when crossed with to
  */
case class CurvedConnection(from: Line, start: Line, to: Line, end: Line, classNumber: Int) extends Connection
case class StraightConnection(line: Line, classNumber: Int) extends Connection

/* line     - the straight travel direction
 * stopLine - the line at which to stop (intersection point with line is the stop point)
 * uk       - flag
 */
case class ConnectionStop(line: Line, stopLine: Line, classNumber: Int, uk: Boolean)

abstract class Intersection {

  val MinDist = 0.5 // half a meter
  val MaxAngle = 0.11 // about 6.3 degree

  def yieldConnections(tt: TT): TraversableOnce[Connection]

  def yieldConnectionStops(tt: TT): TraversableOnce[ConnectionStop]

  /** Calculates the points of a curve with a straight start and end section.
    */
  private def drawCurve(start: Point, curveStart: Point, curveEnd: Point, end: Point): Points = {
    val l1 = Line(start, curveStart)
    val l2 = Line(curveEnd, end)
    val middle = l1 intersect l2
    if (middle == curveStart || middle == curveEnd || curveStart == curveEnd) {
      Seq(start, middle, end)
    } else {
      val curve = curveFromTriangle(curveStart, l1 intersect l2, curveEnd)
      val ps = segmentCurve(MinDist, MaxAngle, curve)
      start +: ps :+ end
    }
  }

  /** format of stop points:
    *   Line(entry point, stop point) if uk=false
    *   Line(stop point, exit point) if uk=true
    */
  private def buildSc4Path(pss: Seq[(Points, TT, Int)], stopPoints: Seq[(Line, TT, Int, Boolean)]): Sc4Path = {
    def card(p: Point): Cardinal = {
      require(p.x.abs != 8 || p.y.abs != 8, "corner points not allowed")
      p match {
        case Point(8,_,_) => East
        case Point(-8,_,_) => West
        case Point(_,8,_) => North
        case Point(_,-8,_) => South
      }
    }
    val paths: immutable.Seq[Path] = pss.map { case (ps, tt, cn) =>
      Path(comment = None, transportType = tt, classNumber = cn,
        entry = card(ps.head), exit = card(ps.last),
        coords = ps.map(pointToCoord))
    }
    val stopPaths: immutable.Seq[StopPath] = stopPoints.map { case (Line(a, b), tt, cn, uk) =>
      StopPath(comment = None, transportType = tt, classNumber = cn, uk = uk,
        entry = if (uk) card(b) else card(a),
        exit = Cardinal.Special,
        coord = if (uk) a else b)
    }
    Sc4Path(paths = paths, stopPaths = stopPaths, terrainVariance = false) // TODO set terrain variance?
  }

  def buildSc4Path: Sc4Path = {
    val pss: Seq[(Points, TT, Int)] = TT.values.toSeq.flatMap { tt =>
      yieldConnections(tt).toSeq flatMap {
        case CurvedConnection(from, start, to, end, cn) =>
          Trimming.trimToTile(drawCurve(from.p1, from intersect start, to intersect end, to.p2)).map((_, tt, cn))
        case StraightConnection(line, cn) =>
          Trimming.trimToTile(Seq(line.p1, line.p2)).map((_, tt, cn))
      }
    }
    val stopPoints: Seq[(Line, TT, Int, Boolean)] = TT.values.toSeq.flatMap { tt =>
      yieldConnectionStops(tt).toSeq flatMap {
        case ConnectionStop(line, stopLine, cn, uk) => {
          val interruptedLine = if (!uk) Seq(line.p1, line intersect stopLine) else Seq(line intersect stopLine, line.p2)
          Trimming.trimToTile(interruptedLine) map { qs =>
            require(qs.size == 2, "stop points should not be on tile boundary")
            (Line(qs(0), qs(1)), tt, cn, uk)
          }
        }
      }
    }
    buildSc4Path(pss, stopPoints)
  }

}
