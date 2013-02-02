package round1
import util._
import math.BigInt

object Prob1 extends Logging with CmdlineInput {
  import BigInt._

  def split(str: String) = str split ' '
  def toInts(str: String) = split(str) map (Integer parseInt _)

  val constantModInt: Int = 1000 * 1000 * 1000 + 7
  val constantMod: BigInt = constantModInt

  def fact(n: Int, k: Int): BigInt = {
    var res: BigInt = 1
    for (i <- 1 to k) {
      res = (res * (n - i + 1)) * (i modInverse constantMod)
    }
    res % constantMod
  }
  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    processInput(lines.grouped(2).toSeq, m) { linePair =>
      val nk = toInts(linePair(0))
      val (n, k) = (nk(0), nk(1))
      val a: Seq[Int] = toInts(linePair(1))
      assert(a.length == n)
      val b = a.sorted(Ordering[Int].reverse) //O (n log n)

      //O(n) calculation - done one time only.
      //This needs to be a BigInt because it will get huge.
      var currFact: BigInt = fact(n - 1, k - 1)

      //Compute a new factorial in O(1) time.
      def nextFact(i: Int): BigInt =
        currFact * (n - k - i) * ((n - i - 1) modInverse constantMod) % constantMod

      (for ((el, i) <- b.zipWithIndex.take(n - k + 1))
      yield {
        //println((el, i, currFact, fact(n - 1 - i, k - 1)))
        //This assertion is not constant-time.
        //assert(currFact == fact(n - 1 - i, k - 1))

        //Warning: we must do this multiplication mod constantMod. We
        //need to use at least Longs.
        val res = el * currFact % constantMod
        currFact = nextFact(i) //O(1)
        res
      }).fold[BigInt](0)((a, b) => (a + b) % constantMod).toString //O(n)
    }
  }
}

/*
import round1.Prob1._
assert(fact(100,50)-fact(99,50)-fact(99,49) % constantMod == 0)
 */
