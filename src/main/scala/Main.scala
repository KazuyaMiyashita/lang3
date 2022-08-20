import scala.io.Source
import lang3.*

object Main {

  def main(args: Array[String]): Unit = {
    val input = if (args.length == 1) {
      val s = Source.fromFile(args(0))
      try {
        s.getLines().mkString("\n")
      } finally {
        s.close()
      }
    } else {
      throw new IllegalArgumentException("filename required")
    }
    Interpreter.run(Parser.run(input).get)
  }

}
