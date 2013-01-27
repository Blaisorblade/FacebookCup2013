import io.Source
import collection.mutable.ArrayBuffer
import language.postfixOps

trait Logging {
  val debug = true
  def println(msg: Any) = if (debug) Console.err.println(msg)
}

object Main extends Logging {
  type Permutation = Array[Int]
  type Histogram = Array[Int]
  type HistogramMap = Map[Char, Int]

  //Integers from 26 to 1.
  val perfectPermutation: Permutation = (26 to (1, -1)).toArray //Array.iterate(26, 26)(_ - 1)

  def beauty(beautyAssignment: Permutation, wordHist: Histogram): Int =
    beautyAssignment zip wordHist map {
      case (beauty, frequence) => beauty * frequence
    } sum;

  def getMaxBeauty(wordHist: HistogramMap): Int = {
    val sortedHist = (wordHist.toArray sortBy (_._2))(Ordering[Int].reverse)
    println(sortedHist.toSeq)
    beauty(perfectPermutation, sortedHist map (_._2))
  }

  def main(args: Array[String]) {
    val inp =
      if (args.length > 0)
        Source.fromFile(args(0))
      else
        Source.stdin
    val lines = inp.getLines

    val m = Integer.parseInt(lines.next) //at most 50.
    for (i <- 1 to m) {
      val line = lines.next //max length: 500
      val histogramMap = buildHistogramMap(line)
      println(histogramMap)
      val maxBeauty = getMaxBeauty(histogramMap)
      Console.println(s"Case #${i}: ${maxBeauty}")
    }
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
/*
  def maxBeauty(wordHist: Histogram): Int = {
    (for (p <- beautyAssignments) yield beauty(p, wordHist)).max
  }

  private def buildHistogramArray(line: String): Histogram = {
    val histogram = buildHistogramMap(line)
    val sortedHistogram = (histogram toSeq) sortBy (_._1) 
    println(sortedHistogram)
    val table = Array.tabulate(26)(idx => histogram.getOrElse((idx + 'a').toChar, 0))
    //println(table)
    println("")

    table
  }
* 
*/