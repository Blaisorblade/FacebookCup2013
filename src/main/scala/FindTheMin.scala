import java.lang.Long
import language.postfixOps
import scala.collection.IndexedSeqLike
import scala.collection.mutable.ArrayBuffer

/*
class RingBuffer[T](buf: Array[T], override val length: Int) extends IndexedSeq[T] with IndexedSeqLike[T, RingBuffer[T]] {
  var offset = 0
  def apply(idx: Int) = buf(idx + offset % length)
  def +=(el: T)
}
*/

class RingBuffer[T](buf: Array[T], val length: Int) {
  private var offset = 0

  private def idxFor(idx: Int): Int =
    idx + offset % length

  def apply(idx: Int) = buf(idxFor(idx))

  def +=(el: T) = {
    buf(idxFor(0)) = el
    offset += 1
  }
}

object FindTheMin extends CmdlineInput with Logging {
  /*
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
  */

  def fillArray(a: Long, b: Long, c: Long, r: Long, k: Int) = {
    def next(state: Long) = b * state + c % r
    ArrayBuffer.iterate(a, k)(next) 
  }

  def split(str: String) = str split ' '
  def toLongs(str: String) = split(str) map (Long parseLong _)
  def toInts(str: String) = split(str) map (Integer parseInt _)
  def main(args: Array[String]) {
    val (lines, t) = getInputAndCount(args) //t is at most 50.
    processInput(lines.grouped(2).toSeq, t) { linePair =>
      val nk = toInts(linePair(0))
      val (n, k) = (nk(0), nk(1))
      val rngParams = toLongs(linePair(1))
      val (a, b, c, r) = (rngParams(0), rngParams(1), rngParams(2), rngParams(3))
      val initialArray = fillArray(a, b, c, r, k)
      ""
    }
  }
}
