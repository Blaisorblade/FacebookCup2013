package qualification

import util._
import io.Source
import collection.mutable.ArrayBuffer
import language.postfixOps

object Main extends Logging with CmdlineInput {
  type Permutation = Array[Int]
  type Histogram = Array[Int]
  type HistogramMap = Map[Char, Int]

  //Integers from 26 to 1.
  val perfectPermutation: Permutation = (26 to (1, -1)).toArray
  //My testcases would not have caught me if I had, say, used the 'until' operator,
  //giving me numbers from 26 to 2, instead of the 'to' operator. But of course it would
  //have made a difference for the real input.
  println(perfectPermutation.toSeq)

  //Simple vector dot product.
  def beauty(beautyAssignment: Permutation, wordHist: Histogram): Int =
    beautyAssignment zip wordHist map {
      case (beauty, frequence) => beauty * frequence
    } sum;

  def getMaxBeauty(wordHist: HistogramMap): Int = {
    val sortedHist: Array[(Char, Int)] = (wordHist.toArray sortBy (_._2))(Ordering[Int].reverse)
    println(sortedHist.toSeq)
    /* This solves the problem by using the [Rearrangement inequality](http://en.wikipedia.org/wiki/Rearrangement_inequality)*/
    beauty(perfectPermutation, sortedHist map (_._2))
  }

  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args) //m is at most 50.
    processInput(lines, m) {
      line =>
        val histogramMap = buildHistogramMap(line)
        println(histogramMap)
        getMaxBeauty(histogramMap)
    }
  }
  
  private def buildHistogramMap(line: String): HistogramMap = {
    println(line)
    val letters = for {
      ch <- line
      if ch.isLetter
    } yield ch.toLower
    letters groupBy identity mapValues (_.length)
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
