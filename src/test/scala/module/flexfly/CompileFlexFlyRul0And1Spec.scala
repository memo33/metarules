package metarules
package module.flexfly

import CompileFlexFlyRul0And1._
import resource._
import org.scalatest.{WordSpec, Matchers}

class CompileFlexFlyRul0And1Spec extends WordSpec with Matchers {

  "FlexFly RUL1 falsies" should {
    "be permanent" in {
      val file = new java.io.File("src/test/resources/FlexFlyRUL1.txt")
      for (scanner <- managed(new java.util.Scanner(file))) {
        val previousFalsies = collection.JavaConversions.asScalaIterator(scanner).filter(_.nonEmpty)

        val resolve = new FlexFlyResolver
        val currentFalsies = for {
          seg <- flexFlySegs.iterator
          idTile = resolve(meta.Tile(seg))
          falsie = convertVirtualTile(seg)
          line <- rul1Entry(falsie, idTile.id, seg.toString).lines
          if line.nonEmpty
        } yield line

        assert(previousFalsies sameElements currentFalsies,
          "FlexFly falsies were different from specification, but they should NEVER change!")
      }
    }
  }

}
