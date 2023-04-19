package io.github.memo33
package metarules

import io.github.memo33.scdbpf
import scala.language.implicitConversions

/** Contains internal MetaRUL implementation (mostly); changes in this package
  * should not usually be necessary; for the code
  * generating mechanism, [[Syntax#IdResolver]] and [[Syntax#RuleGenerator]] need to be
  * implemented.
  */
package object meta {

  type IdTile = internal.IdTile

  implicit def metaRFToScdbpfRF(rf: meta.RotFlip): scdbpf.DbpfUtil.RotFlip = {
    scdbpf.DbpfUtil.RotFlip(rf.rot, rf.flip)
  }

  implicit def scdbpfRFToMetaRF(rf: scdbpf.DbpfUtil.RotFlip): meta.RotFlip = {
    meta.RotFlip(rf.rot, rf.flip)
  }

}

