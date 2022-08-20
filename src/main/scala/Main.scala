object Main {

  def main(args: Array[String]): Unit = {
    val input = if (args.length == 1) {
      args(0)
    } else {
      throw new IllegalArgumentException("Enter the expression. e.g. \"2 * (3 + 7)\"")
    }
    val result = dentaku.Parser.run(input)
    println(result)
  }

}
