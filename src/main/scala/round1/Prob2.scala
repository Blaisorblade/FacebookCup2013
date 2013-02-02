package round1
import util._

object Prob2 extends Logging with CmdlineInput {
  def compatibleChars(ch1: Char, ch2: Char) = ch1 == '?' || ch2 == '?' || ch1 == ch2
  def compatibleStrings(s1: String, s2: String) = s1 zip s2 forall (compatibleChars _).tupled

  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    processInput(lines.grouped(3).toSeq, m) { testCase =>
      val m = Integer parseInt testCase(0)
      val l = testCase(1).length / m
      val k1 = (testCase(1) grouped l).toArray.toSeq
      val k2 = (testCase(2) grouped l).toArray.toSeq
      println((k1, k2))
      ""
    }
  }
}
