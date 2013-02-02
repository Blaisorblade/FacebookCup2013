package round1
import util._

object Prob1 extends Logging with CmdlineInput {
  def split(str: String) = str split ' '
  def toInts(str: String) = split(str) map (Integer parseInt _)

  def fact(n: Int, k: Int): Long = {
    var res: Long = 1
    for (i <- 1 to k) {
      res = (res * (n - i + 1)) / i
    }
    res
  }
  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    processInput(lines.grouped(2).toSeq, m) { linePair =>
      val nk = toInts(linePair(0))
      val (n, k) = (nk(0), nk(1))
      val a: Seq[Int] = toInts(linePair(1))
      assert(a.length == n)
      val b = a.sorted(Ordering[Int].reverse) //O (n log n)
      var currFact = fact(n - 1, k - 1)
      def nextFact(i: Int) = currFact * (n - k - i) / (n - i - 1)
      (for ((el, i) <- b.zipWithIndex.take(n - k + 1))
      yield {
        //println((el, i, currFact, fact(n - 1 - i, k - 1)))
        assert(currFact == fact(n - 1 - i, k - 1)) //This assertion is not constant-time.
        val res = el * currFact //O(1)
        currFact = nextFact(i) //O(1)
        res
      }).sum.toString //O(n)
    }
  }
}
