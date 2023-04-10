package metarules.meta

trait RuleGeneratorSyntax { syntax: Syntax =>

/** Needs to be implemented for generating RUL2 code for a specific project,
  * along with an [[Syntax#IdResolver]].
  */
trait RuleGenerator {
  /** The implementation of this is likely to change. After [[start]] has been
    * called, the generated Rules can be fetched from this queue.
    */
  val queue = new scala.collection.mutable.Queue[Rule[IdTile]]()
  def % = Rule.CopyTile
  /** A buffer for Rules to collect Rules before flushing them via
    * [[createRules]].
    */
  protected val Rules = new RulesBuffer
  /** When called, the Rules in the buffer will be flushed, but duplicate Rules
    * in the buffer will be omitted.
    */
  protected def createRules(): Unit = {
    Rules.distinct foreach { r =>
      queue ++= RuleTransducer(r)(context)
    }
    Rules.clear()
  }

  /** The context provides the resolver (the function that maps `Tiles` (meta
    * syntax) to `IdTiles` (IID, Rot, Flip).
    *
    * It also provides the `tileOrientationCache`, which
    * overwrites the possible absolute orientations of some IDs when the
    * `RuleTransducer` determines that this is necessary.
    * For correct functionality, it is important to overwrite this by a map that
    * is common between all rule generators (globally).
    *
    * It also implements handling of mirror variants.
    */
  var context: RuleTransducer.Context

  /** Generates the Rules. Needs to be implemented.*/
  def start(): Unit
}

private[meta] class RulesBuffer extends scala.collection.mutable.ArrayBuffer[Rule[SymTile]] {
  def += (rules: (Rule[SymTile], Rule[SymTile])): this.type = {
    this += rules._1
    this += rules._2
  }
}

} // end of RuleGeneratorSyntax
