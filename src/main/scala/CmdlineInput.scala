import io.Source

trait CmdlineInput {
  def getInputAndCount(args: Array[String]): (Array[String], Int) = {
    val inp =
      if (args.length > 0)
        Source.fromFile(args(0))
      else
        Source.stdin
    val lines: Iterator[String] = inp.getLines

    val m = Integer.parseInt(lines.next)
    (lines.toArray, m)
  }
  def processInput[S, T](lines: Seq[S], m: Int)(processor: S => T) {
    for (i <- 1 to m)
      Console.println(s"Case #${i}: ${processor(lines(i - 1))}")
  }
}