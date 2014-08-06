
/** Contains internal MetaRUL implementation (mostly); changes in this package
  * are usually only required for adding additional flags; for the code
  * generating mechanism, [[IdResolver]] and [[RuleGenerator]] need to be
  * implemented.
  */
package object meta {

  /** The function that maps `Tiles` (meta syntax) to `IdTiles` (IID, Rot, Flip).
    */
  type IdResolver = PartialFunction[Tile, IdTile]
}

