import io.Source
import collection.mutable.ArrayBuffer
import language.postfixOps

object Main {
  type Permutation = Array[Int]
  type Histogram = Array[Int]
  type HistogramMap = Map[Char, Int]

  def beauty(beautyAssignment: Permutation, wordHist: Histogram): Int = {
    beautyAssignment zip wordHist map {case (beauty, frequence) => beauty * frequence} sum;
  }

  /*
  val beautyAssignments: Array[Permutation] = Array.empty
  */
  //Integers from 26 to 1.
  val perfectPermutation: Permutation = (26 to (1, -1)).toArray //Array.iterate(26, 26)(_ - 1)

  /*def maxBeauty(wordHist: Histogram): Int = {
    (for (p <- beautyAssignments) yield beauty(p, wordHist)).max
  }*/
  def getMaxBeauty(wordHist: HistogramMap): Int = {
    val sortedHist = (wordHist.values.toArray sorted) reverse;
    val sortedHist2 = wordHist.values.toArray.sorted(Ordering[Int].reverse)
    assert(sortedHist.toSeq == sortedHist2.toSeq)
    println(sortedHist.toSeq)
    println(perfectPermutation.toSeq)
    val res = beauty(perfectPermutation, sortedHist)
    res
  }

  def main(args: Array[String]) {
    val inp =
      if (args.length > 0)
        Source.fromFile(args(0))
      else
        Source.stdin
    val lines = inp.getLines
    val m = Integer.parseInt(lines.next) //at most 50.
    for (i <- 1 to m) yield {
      val line = lines.next //max length: 500
      val histogram = buildHistogramMap(line)
      /*val table = buildHistogramArray(line)
      val maxBeauty = maxBeauty(table)*/
      val maxBeauty = getMaxBeauty(histogram)
      Console.println(s"Case #${i}: ${maxBeauty}")
    }
  }
  
  val debug = true
  def println(msg: Any) = if (debug) Console.err.println(msg)

  private def buildHistogramArray(line: String): Histogram = {
    val histogram = buildHistogramMap(line)
    val sortedHistogram = (histogram toSeq) sortBy (_._1) 
    println(sortedHistogram)
    val table = Array.tabulate(26)(idx => histogram.getOrElse((idx + 'a').toChar, 0))
    //println(table)
    println("")

    table
  }
  
  private def buildHistogramMap(line: String): HistogramMap = {
    println(line)
    val letters = for {
      ch <- line
      if ch.isLetter
    } yield ch.toLower
    println(letters)
    val histogram = letters groupBy identity mapValues (_.length)
    histogram
  }
}