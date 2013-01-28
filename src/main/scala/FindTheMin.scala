import java.lang.Long
import language.postfixOps
import scala.collection.IndexedSeqLike
import scala.collection.mutable
import mutable.{ArrayBuffer}
import java.util.{PriorityQueue, TreeSet}
import collection.convert.decorateAsJava._

class RingBuffer[T](val buf: Array[T], override val length: Int) extends IndexedSeq[T] {
  var offset = 0

  private def idxFor(idx: Int): Int =
    (idx + offset) % length

  def apply(idx: Int) = buf(idxFor(idx))

  def +=(el: T) = {
    buf(idxFor(0)) = el
    offset = (offset + 1) % length
  }
}

object FindTheMin extends CmdlineInput with Logging {
  def histogram(s: Seq[Int]): mutable.Map[Int, Int] =
    mutable.Map() ++= (s groupBy identity mapValues (_ length))
  /*
  def slidingWindow(s: Seq[Long], k: Int) {
    //We need to incrementally maintain this when new elements are added at the end of s,
    //so that the sliding window moves.
    (s takeRight k groupBy identity mapValues (_ length) keySet)
    //What we still don't get is a quick way to find the first non-zero element.
    //That requires a priority queue - in pseudocode:
    //(s takeRight k groupBy identity mapValues (_ length) keySet) toPriorityQueue
    //But we need a priority queue with the complement of the above!  
  }
  */

  //Note: we want intermediate results of computations involving b, c, r to use Longs; hence we make them Long,
  //so that they are upcast at the call site.
  def fillArray(a: Int, b: Long, c: Long, r: Long, k: Int) = {
    def next(state: Int) = ((b * state + c) % r).toInt
    ArrayBuffer.iterate(a, k)(next) 
  }

  def split(str: String) = str split ' '
  def toLongs(str: String) = split(str) map (Long parseLong _)
  def toInts(str: String) = split(str) map (Integer parseInt _)

  def updateMap[K, V](m: mutable.Map[K, V], k: K)(updater: V => V) {
    m += (k -> updater(m(k))) 
  }

  def updateMapWithDefaultOld[K, V](m: mutable.Map[K, V], k: K, defaultOld: V)(updater: V => V) {
    m += (k -> updater(m getOrElse (k, defaultOld))) 
  }

  def main(args: Array[String]) {
    val (lines, t) = getInputAndCount(args) //t is at most 50.
    processInput(lines.grouped(2).toSeq, t) { linePair =>
      //Define parameters.
      val nk = toInts(linePair(0))
      val (n, k) = (nk(0), nk(1))
      val rngParams: Array[Int] = toInts(linePair(1))
      val (a, b, c, r) = (rngParams(0), rngParams(1), rngParams(2), rngParams(3))

      //Define initial state.
      val initialArray = fillArray(a, b, c, r, k)
      val ringBuffer = new RingBuffer(initialArray.toArray, k)
      val hist = histogram(initialArray)
      //Now we need a priority queue, but of [0, 10^9] \ initialArray (where \ is set subtraction). Ouch!
      //But luckily, since we only ever want the min, we can just as well maintain [0, k] \ initialArray.
      val initialPrioQContent = 0 to k filterNot hist.contains
      val priorityQueue = new PriorityQueue(initialPrioQContent.asJava)

      var idx = k
      while (idx < ((2 * k) min n)) {
        val minAbsent = (priorityQueue peek)
        val disappearing = ringBuffer(0)
        if (disappearing != minAbsent) {
          //We need to update the state
          updateMap(hist, disappearing)(_ - 1)
          if (disappearing <= k && hist(disappearing) == 0) {
            priorityQueue offer disappearing
          }
          updateMapWithDefaultOld(hist, minAbsent, 0)(_ + 1)
          if (minAbsent <= k && hist(minAbsent) == 1)
            priorityQueue remove minAbsent
        }
        ringBuffer += minAbsent
        idx += 1
      }
      val missingElementTransition = (priorityQueue peek)
      val smartRB =
        if (idx == 2 * k) {
          assert(ringBuffer.offset == 0)
          new RingBuffer(ringBuffer.buf :+ missingElementTransition, k + 1)
        } else {
          //offset here won't be 0 iff we ended with idx != 2*k. But that's the slow path, so we can afford slightly less efficiency.
          new RingBuffer(ringBuffer.toSeq :+ missingElementTransition toArray, k + 1)
          //In fact, I think we can afford it anyway, but let's play it safe.
        }
      //Inner loop. We skip k positions at a time.
      //Invariant: positions 0 to k - 1 are the last k values, position k is the missing one.
      while (idx < n - k) {
        idx += k
        //This encodes:
        // smartRB.offset -= 1
        smartRB.offset += k //position k becomes 0 and goes to be the first, old position k - 1 becomes k and goes to be the new missing one.
      }
      //For the last few iterations, we continue using the standard algorithm - with the difference that the priority queue is collapsed to
      //a single element, the one in 0..k which is not currently in the queue itself.
      var missingElement = smartRB(k)
      val newRB = new RingBuffer(smartRB take k toArray, k)
      while (idx < n) {
        val minAbsent = missingElement
        missingElement = newRB(0)
        newRB += minAbsent
        idx += 1
      }
      
      //When indexing inside ringBuffer/newRB, we are indexing inside the last k values. 
      newRB(k - 1).toString
    }
  }
}
