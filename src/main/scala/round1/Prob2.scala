package round1
import util._
import collection.mutable

object Prob2 extends Logging with CmdlineInput {
  def compatibleChars(ch1: Char, ch2: Char) = ch1 == '?' || ch2 == '?' || ch1 == ch2
  def compatibleStrings(s1: String, s2: String) = s1 zip s2 forall (compatibleChars _).tupled
  def noWildcard(s: String): Boolean = !(s contains '?')

  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    processInput(lines.grouped(3).toSeq, m)(processTestCase)
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
    val solution = mutable.HashMap[Int, Int]()
    def logSolWasSet(j: Int) = println(s"${j} -> ${solution(j)}")

    val freeLocationsK1 = mutable.HashMap() ++= k1Map
    for {
      str <- (k1 filter noWildcard).toSet intersect (k2 filter noWildcard).toSet
      j <- k2Map(str)
      currPosK1: List[Int] = freeLocationsK1(str)
      if currPosK1.nonEmpty
    } {
      solution(j) = currPosK1.head
      logSolWasSet(j)
      freeLocationsK1(str) = currPosK1.tail
    }

    val compatible = Array.tabulate(m, m) {
        (j, i) => compatibleStrings(k2(j), k1(i))
      }

    for {
      j <- 0 until m
      if !(solution contains j)
      possibleAssignments = compatible(j).count(identity)
      if possibleAssignments <= 1
    } {
      if (possibleAssignments == 1) {
        solution(j) = compatible(j) indexOf true
        for (otherJ <- 0 until m)
          compatible(solution(j))(otherJ) = false
        logSolWasSet(j)
      } else {
        return "IMPOSSIBLE"
      }
    }

      ""
  }
}
