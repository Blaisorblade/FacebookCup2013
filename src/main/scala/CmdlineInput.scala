import io.Source

trait CmdlineInput {
  def getInputAndCount(args: Array[String]): (Array[String], Int) = {
    val inp =
      if (args.length > 0)
        Source.fromFile(args(0))
      else
        Source.stdin
    val lines: Array[String] = inp.getLines.toArray

    val m = Integer.parseInt(lines(0))
    (lines, m)
  }
  def processInput[T](lines: Array[String], m: Int)(processor: String => T) {
    for (i <- 1 to m)
      Console.println(s"Case #${i}: ${processor(lines(i))}")
  }
}