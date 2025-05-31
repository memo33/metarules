package io.github.memo33
package metarules.meta

import scala.collection.mutable.{ArrayBuffer, Builder}

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
  protected val Rules = new RulesBuilder
  /** When called, the Rules in the buffer will be flushed, but duplicate Rules
    * in the buffer will be omitted.
    */
  protected def createRules(): Unit = {
    queue ++= Rules.result()
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
    * It also implements a means of preprocessing the input rules, e.g. for
    * dealing with mirror variants.
    */
  var context: RuleTransducer.Context

  /** Generates the Rules. Needs to be implemented.*/
  def start(): Unit

  private[meta] class RulesBuilder extends Builder[Rule[SymTile], Iterator[Rule[IdTile]]] {
    val buffer = ArrayBuffer.empty[(Rule[SymTile], Seq[Rule[IdTile]])]
    def clear(): Unit = buffer.clear()
    def result(): Iterator[Rule[IdTile]] = buffer.distinctBy(_._1).iterator.flatMap(_._2)

    def addOne(rule: Rule[SymTile]): this.type = {
      // We directly transduce the rule so that, if some tile ID fails to be
      // resolved in a Resolver, an exception will be thrown immediately, with a
      // stack trace pointing to the location where the rule was defined and
      // added to the RulesBuilder.
      val resolvedRules = try {
        RuleTransducer(rule)(context).toSeq
      } catch {
        case e: RuleTransducer.ResolutionFailed =>
          // rethrow a new exception in order to shorten the stack trace so that
          // actual problematic call is closer to the top
          val dummy = new RuleTransducer.ResolutionFailed(e.tile, e.rule, e.reason, frame = None)
          val trace =
            dummy.getStackTrace.dropWhile(elem =>
              elem.getClassName.startsWith("scala.") ||
              elem.getClassName.startsWith("io.github.memo33.metarules.meta.RuleGeneratorSyntax$RuleGenerator$RulesBuilder"))
          val e2 = new RuleTransducer.ResolutionFailed(e.tile, e.rule, e.reason, frame = trace.headOption)
          e2.setStackTrace(trace)
          throw e2
      }
      buffer += ((rule, resolvedRules))
      this
    }

    def += (rules: (Rule[SymTile], Rule[SymTile])): this.type = {
      this += rules._1
      this += rules._2
    }
  }

}

} // end of RuleGeneratorSyntax
