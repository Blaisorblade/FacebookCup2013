import java.lang.Long
import language.postfixOps

object FindTheMin extends CmdlineInput {
  def slidingWindow(s: Seq[Long], k: Int) = {
    //(s takeRight k zipWithIndex) groupBy (_._2)

    //We need to incrementally maintain this when new elements are added at the end of s,
    //so that the sliding window moves.
    ((s takeRight k groupBy identity mapValues (_ length)) keySet)
    //What we still don't get is a quick way to find the first non-zero element.
    //That requires a priority queue - in pseudocode:
    //(s takeRight k groupBy identity mapValues (_ length) keySet) toPriorityQueue  
    ()
  }

  def toLongs(str: String) = str split ' ' map (Long parseLong _)
  def main(args: Array[String]) {
    val (lines, t) = getInputAndCount(args) //t is at most 50.
    processInput(lines.grouped(2).toSeq, t) { linePair =>
      val nk = toLongs(linePair(0))
      val (n, k) = (nk(0), nk(1))
      val rngParams = toLongs(linePair(1))
      val (a, b, c, r) = (rngParams(0), rngParams(1), rngParams(2), rngParams(3))
      ""
    }
  }
}
