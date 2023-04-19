package metarules

import io.github.memo33.scdbpf

package object pathing {
  import Bezier.Point
  import scala.language.implicitConversions

  type Points = Seq[Point]

  // These are contained in package `meta`.
  // implicit def metaRFToScdbpfRF(rf: meta.RotFlip): scdbpf.DbpfUtil.RotFlip = {
  //   scdbpf.DbpfUtil.RotFlip(rf.rot, rf.flip)
  // }

  // implicit def scdbpfRFToMetaRF(rf: scdbpf.DbpfUtil.RotFlip): meta.RotFlip = {
  //   meta.RotFlip(rf.rot, rf.flip)
  // }

  implicit def coordToPoint(c: scdbpf.Sc4Path.Coord): Point = Point(c._1, c._2, c._3)
  implicit def pointToCoord(p: Point): scdbpf.Sc4Path.Coord = (p.x, p.y, p.z)
}
