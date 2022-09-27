package metarules.pathing

import scala.collection._
import Bezier._
import scdbpf.Sc4Path, Sc4Path.{TransportType => TT, _}, Cardinal._

abstract class Connection
/** Curved connection in the following format:
  * from  – the from-direction
  * start - the line that determines the start of the curve if crossed with from
  * to    - the to-direction
  * end   - the line that deterimens the end point of the curve when crossed with to
  */
case class CurvedConnection(from: Line, start: Line, to: Line, end: Line) extends Connection
case class StraightConnection(line: Line) extends Connection

abstract class Intersection {

  val MinDist = 0.5 // half a meter
  val MaxAngle = 0.11 // about 6.3 degree

  def yieldConnections(tt: TT): TraversableOnce[Connection]

  // TODO implement stop points
  //def yieldStopPoints: TraversableOnce[(Point, Cardinal, TT)]

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

  private def buildSc4Path(pss: Seq[(Points, TT)]): Sc4Path = {
    def card(p: Point): Cardinal = {
      require(p.x.abs != 8 || p.y.abs != 8, "corner points not allowed")
      p match {
        case Point(8,_,_) => East
        case Point(-8,_,_) => West
        case Point(_,8,_) => North
        case Point(_,-8,_) => South
      }
    }
    val paths: immutable.Seq[Path] = pss.map { case (ps, tt) =>
      Path(comment = None, transportType = tt, classNumber = 0,
        entry = card(ps.head), exit = card(ps.last),
        coords = ps.map(pointToCoord)(breakOut))
    } (breakOut)
    Sc4Path(paths = paths, terrainVariance = false).renumberClassNumbers // TODO set terrain variance?
  }

  def buildSc4Path: Sc4Path = {
    val pss: Seq[(Points, TT)] = TT.values.toSeq.flatMap { tt =>
      yieldConnections(tt).toSeq flatMap {
        case CurvedConnection(from, start, to, end) =>
          Trimming.trimToTile(drawCurve(from.p1, from intersect start, to intersect end, to.p2))
        case StraightConnection(line) =>
          Trimming.trimToTile(Seq(line.p1, line.p2))
      } map ((_, tt))
    }
    buildSc4Path(pss)
  }

}
