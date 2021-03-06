package metarules.meta

/** Needs to be implemented for generating RUL2 code for a specific project,
  * along with an [[IdResolver]].
  */
trait RuleGenerator {
  /** The implementation of this is likely to change. After [[start]] has been
    * called, the generated Rules can be fetched from this queue.
    */
  val queue = new scala.collection.mutable.Queue[Rule[IdTile]]()
  def % = Tile.CopyTile
  /** A buffer for Rules to collect Rules before flushing them via
    * [[createRules]].
    */
  protected val Rules = new RulesBuffer
  /** When called, the Rules in the buffer will be flushed, but duplicate Rules
    * in the buffer will be omitted.
    */
  protected def createRules(): Unit = {
    Rules.distinct foreach { r =>
      queue ++= RuleTransducer(r)(resolver)
    }
    Rules.clear()
  }

  /** The function that maps `Tiles` (meta syntax) to `IdTiles` (IID, Rot, Flip). */
  val resolver: IdResolver
  /** Generates the Rules. Needs to be implemented.*/
  def start(): Unit
}

private[meta] class RulesBuffer extends scala.collection.mutable.ArrayBuffer[Rule[Tile]] {
  def += (rules: (Rule[Tile], Rule[Tile])): this.type = {
    this += rules._1
    this += rules._2
  }
}
