package round1
import util._
import collection.mutable

object Prob2 extends Logging with CmdlineInput {
  def compatibleChars(ch1: Char, ch2: Char) = ch1 == '?' || ch2 == '?' || ch1 == ch2
  def compatibleStrings(s1: String, s2: String) = s1 zip s2 forall (compatibleChars _).tupled
  //ch1 better than ch2?
  def betterChar(ch1: Char, ch2: Char) = ch2 == '?' || ch1 == ch2
  def betterString(s1: String, s2: String) = s1 zip s2 forall (betterChar _).tupled
  def noWildcard(s: String): Boolean = !(s contains '?')

  def sieveTheWorse[T](v: List[T])(cmp: T => T => Boolean): List[T] = {
    def sieve(s: List[T]): List[T] =
      s match {
        case h :: t =>
          val newTail = sieve(t) filterNot cmp(h)
          if (t exists (cmp(_)(h)))
            newTail
          else
            h :: newTail
        case Nil => Nil
      }
    sieve(v)
  }
  def sieveTheWorseStr(strs: List[String]) = sieveTheWorse(strs)((betterString _).curried)
  println(sieveTheWorseStr(List("ab?", "a??", "?b?", "ab?")))
  println(sieveTheWorseStr(List("a??", "?b?", "ab?")))
  assert(sieveTheWorseStr(List("a??", "?b?", "ab?")) == "ab?" :: Nil)
  assert(sieveTheWorseStr(List("a??", "?b?", "ab?", "??c")) == "ab?" :: "??c" :: Nil)

  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    processInput(lines.grouped(3).toSeq, m)(processTestCase)
  }

  class Solution(
      val j2i: mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int](),
      val i2j: mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()) {
    def update(j: Int, i: Int) = {
      j2i(j) = i
      println(s"${j} -> ${i}")
      i2j(i) = j
    }
  }

  def processTestCase(testCase: Array[String]): String = {
    val m = Integer parseInt testCase(0)
    val l = testCase(1).length / m
    val k1 = (testCase(1) grouped l).toList
    val k2 = (testCase(2) grouped l).toList

    println("")
    println((k1, k2))

    def str2LocationMap(k: List[String]) = k.zipWithIndex.groupBy(_._1).toMap mapValues (_ map (_._2))
    val k1Map = str2LocationMap(k1)
    val k2Map = str2LocationMap(k2)

    //Map the index of a section of k2 to its position in k1.
    val solution = new Solution

    val freeLocationsK1 = mutable.HashMap() ++= k1Map
    for {
      str <- (k1 filter noWildcard).toSet intersect (k2 filter noWildcard).toSet
      j <- k2Map(str)
      currPosK1: List[Int] = freeLocationsK1(str)
      if currPosK1.nonEmpty
    } {
      solution(j) = currPosK1.head
      freeLocationsK1(str) = currPosK1.tail
    }

    val compatible_j2i = Array.tabulate(m, m) {
        (j, i) => compatibleStrings(k2(j), k1(i))
      }
    //Transpose
    val compatible_i2j = Array.tabulate(m, m) {
        (i, j) => compatible_j2i(j)(i)
      }

    def compatibleAndFree_j2i(j: Int)(i: Int): Boolean = compatible_j2i(j)(i) && !(solution.j2i contains j) && !(solution.i2j contains i)
    def compatibleAndFree_j2iView(j: Int) = (0 until m).view map compatibleAndFree_j2i(j)
    def compatibleAndFree_i2jView(i: Int) = (0 until m).view map (compatibleAndFree_j2i(_: Int)(i))
      //((j: Int) => compatibleAndFree_j2i(j)(i)/*compatible_i2j(i)(j) && !(solution.j2i contains j) && !(solution.i2j contains i)*/)

    var stuffChanged = true
    while (stuffChanged) {
      stuffChanged = false
      for {
        j <- 0 until m
        if !(solution.j2i contains j)
        possibleAssignments = compatibleAndFree_j2iView(j).count(identity)
        if possibleAssignments <= 1
      } {
        if (possibleAssignments == 1) {
          stuffChanged = true
          solution(j) = compatibleAndFree_j2iView(j) indexOf true
        } else {
          return "IMPOSSIBLE"
        }
      }

      for {
        i <- 0 until m
        if !(solution.i2j contains i)
        possibleAssignments = compatibleAndFree_i2jView(i).count(identity)
        if possibleAssignments <= 1
      } {
        if (possibleAssignments == 1) {
          stuffChanged = true
          solution(compatibleAndFree_i2jView(i) indexOf true) = i
        } else {
          return "IMPOSSIBLE"
        }
      }
    }

    ""
  }
}
