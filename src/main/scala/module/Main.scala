package module

import java.io.{File, PrintWriter}
import meta.{RuleGenerator, IdResolver}

/** Usage: Replace (in source code) `resolve` and `generator` by custom
  * implementation, optionally replace `file`, too.
  * Then call `sbt run` to execute.
  */
object Main {

  lazy val resolve: IdResolver = new RhwResolver orElse new MiscResolver orElse new NwmResolver
  lazy val generator: RuleGenerator = new RhwRuleGenerator(resolve)

  lazy val file = new File("target/output.txt")

  def main(args: Array[String]): Unit = start()

  def start(file: File = file, generator: RuleGenerator = generator): Unit = {
    generator.start()
    // TODO to be revised, later, in order to make more efficient
    val printer = new PrintWriter(file)
    try {
      for (rule <- generator.queue) {
        printer.println(rule(0) + "," + rule(1) + "=" + rule(2) + "," + rule(3))
      }
    } finally {
      printer.close()
    }
  }
}
