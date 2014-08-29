package meta

import org.scalatest.{WordSpec, Matchers}
import RotFlip._

class GroupElementSpec extends WordSpec with Matchers {

  "RotFlip/GroupElement" should {
    "have proper apply function" in {
      RotFlip(0,0) should be (R0F0)
      RotFlip(1,0) should be (R1F0)
      RotFlip(2,0) should be (R2F0)
      RotFlip(3,0) should be (R3F0)
      RotFlip(0,1) should be (R0F1)
      RotFlip(1,1) should be (R1F1)
      RotFlip(2,1) should be (R2F1)
      RotFlip(3,1) should be (R3F1)
    }
    "multiply associatively" in {
      for (a <- elements; b <- elements; c <- elements) {
        (a * b) * c should be (a * (b * c))
      }
    }
    "invert correctly" in {
      for (a <- elements) {
        a / a should be (R0F0)
      }
    }
    "have proper neutral element" in {
      for (a <- elements) {
        a * R0F0 should be (a)
        R0F0 * a should be (a)
      }
    }
    "multiply correctly" in {
      R1F0 * R1F0 should be (R2F0)
      R1F0 * R2F0 should be (R3F0)
      R1F0 * R3F0 should be (R0F0)
      R1F0 * R2F1 should be (R3F1)
      R2F0 * R2F1 should be (R0F1)
      R0F1 * R2F1 should be (R2F0)
      R1F1 * R2F1 should be (R3F0)
      R2F1 * R2F1 should be (R0F0)
      R3F1 * R2F1 should be (R1F0)
      R1F1 * R1F1 should be (R0F0)
    }
  }
}

class SymGroupSpec extends WordSpec with Matchers {
  import Group.SymGroup, SymGroup._

  "SymGroup" should {
    "have quotient group of correct order" in {
      for (g <- SymGroup.values) {
        g.size * g.quotient.size should be (8)
      }
    }

    "respect subgroup relation" in {
      for (g <- SymGroup.values) {
        Cyc1 sub g should be (true)
        g sub Dih4 should be (true)
      }
      for (g <- values; h <- values; if g.size == h.size && g != h) {
        g sub h should be (false)
        h sub g should be (false)
      }
      Cyc2A sub Cyc4 should be (true)
      Cyc2B sub Cyc4 should be (false)
      Cyc2B sub Dih2A should be (true)
      Cyc2B sub Dih2B should be (false)
    }

//    "intersect correctly" in {
//      Dih2A intersect Dih2B should be (Cyc2A)
//      Dih2A intersect Cyc4 should be (Cyc2A)
//      Dih2A intersect Cyc2C should be (Cyc1)
//      for (g <- values; h <- values) {
//        val is = g intersect h
//        if (g sub h) is should be (g)
//        if (h sub g) is should be (h)
//        is should be (h intersect g)
//        is sub g should be (true)
//        is sub h should be (true)
//      }
//    }

    "rotate correctly" in {
      Cyc2B * R1F0 should be (Cyc2D)
      Cyc2B * R2F0 should be (Cyc2B)
      Cyc2B * R0F1 should be (Cyc2B)
      Cyc2B * R1F1 should be (Cyc2D)
      Cyc2C * R1F0 should be (Cyc2E)
      Cyc2C * R2F0 should be (Cyc2C)
      Cyc2C * R0F1 should be (Cyc2E)
      Cyc2C * R1F1 should be (Cyc2C)
    }
  }
}
