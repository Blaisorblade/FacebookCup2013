package round1
import util._
import collection.mutable
import language.postfixOps
import scala.util.control.Breaks._

object Prob2 extends Logging with CmdlineInput {
  def compatibleChars(ch1: Char, ch2: Char) = ch1 == '?' || ch2 == '?' || ch1 == ch2
  def compatibleStrings(s1: String, s2: String) = s1 zip s2 forall (compatibleChars _).tupled

  def lowestMergeCh(ch1: Char, ch2: Char) =
    (ch1, ch2) match {
      case ('?', '?') => 'a'
      case ('?', _) => ch2
      case (_, '?') => ch1
      case _ if ch1 == ch2 =>
        ch1
      case _ => throw new IllegalArgumentException("Incompatible characters cannot be merged!")
    }
  def lowestMergeStrings(s1: String, s2: String): String = (s1 zip s2 map (lowestMergeCh _).tupled).mkString
  assert(lowestMergeStrings("?a", "??") == "aa")

  //ch1 better than ch2?
  def betterChar(ch1: Char, ch2: Char) = ch2 == '?' || ch1 == ch2
  def betterString(s1: String, s2: String) = s1 zip s2 forall (betterChar _).tupled
  def noWildcard(s: String): Boolean = !(s contains '?')

  def sieveTheWorse[T](v: List[T])(cmp: T => T => Boolean): List[T] = {
    def sieve(s: List[T]): List[T] =
      s match {
        case h :: t =>
          val newTail = sieve(t) filterNot cmp(h)
          if (newTail exists (cmp(_)(h)))
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
  println(sieveTheWorseStr(List("??", "??")))
  assert(sieveTheWorseStr(List("a??", "?b?", "ab?")) == "ab?" :: Nil)
  assert(sieveTheWorseStr(List("a??", "?b?", "ab?", "??c")) == "ab?" :: "??c" :: Nil)

  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    processInput(lines.grouped(3).toSeq, m)(processTestCase)
  }

  trait Updateable extends ((Int, Int) => Unit) {
    def update(a: Int, b: Int): Unit = apply(a, b)
  }

  trait SolutionView extends Updateable {
    def compatibleAndFreeView(idx: Int): Seq[Boolean]
    def solMatrix: mutable.HashMap[Int, Int]
  }

  class Solution(m: Int, compatible_j2i: Array[Array[Boolean]],
    val j2i: mutable.HashMap[Int, Int] = mutable.HashMap(),
    val i2j: mutable.HashMap[Int, Int] = mutable.HashMap()) {

    def dup: Solution = new Solution(m, compatible_j2i, mutable.HashMap() ++= j2i, mutable.HashMap() ++= i2j)

    def compatibleAndFree_j2i(j: Int)(i: Int): Boolean = compatible_j2i(j)(i) && !(j2i contains j) && !(i2j contains i)
    def compatibleAndFree_j2iView(j: Int) = (0 until m).view map compatibleAndFree_j2i(j)
    def compatibleAndFree_i2jView(i: Int) = (0 until m).view map (compatibleAndFree_j2i(_: Int)(i))

    def j2iview: SolutionView = new SolutionView {
        def solMatrix = j2i
        def compatibleAndFreeView(idx: Int): Seq[Boolean] = compatibleAndFree_j2iView(idx)
        def apply(j: Int, i: Int) = {
          j2i(j) = i
          println(s"${j} -> ${i}")
          i2j(i) = j
        }
      }
    def i2jview: SolutionView = new SolutionView {
        def solMatrix = i2j
        def compatibleAndFreeView(idx: Int): Seq[Boolean] = compatibleAndFree_i2jView(idx)
        def apply(i: Int, j: Int) = {
          j2iview.update(j, i)
        }
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

    val compatible_j2i = Array.tabulate(m, m) {
        (j, i) => compatibleStrings(k2(j), k1(i))
      }
    //Transpose
    val compatible_i2j = Array.tabulate(m, m) {
        (i, j) => compatible_j2i(j)(i)
      }

    val infScore = "z"
    val scores_i2j: Array[Array[String]] = Array.tabulate(m, m) {
        (i, j) => if (compatible_i2j(i)(j)) lowestMergeStrings(k1(i), k2(j)) else infScore
      }

    //Map the index of a section of k2 to its position in k1.
    val solution = new Solution(m, compatible_j2i)

    val freeLocationsK1 = mutable.HashMap() ++= k1Map
    for {
      str <- (k1 filter noWildcard).toSet intersect (k2 filter noWildcard).toSet
      j <- k2Map(str)
      currPosK1: List[Int] = freeLocationsK1(str)
      if currPosK1.nonEmpty
    } {
      solution.j2iview(j) = currPosK1.head
      freeLocationsK1(str) = currPosK1.tail
    }

    def mainLoop(solution: Solution): Option[Solution] = {
      var stuffChanged = true

      //We just need to return the new solution.
      //Now: true = impossible (no new solution), false -> there is a new solution.
      def doAssignments(solution: Solution, solutionViewF: Solution => SolutionView, canBacktrack: Boolean, idx: Int): Option[Solution] = {
        val solutionView = solutionViewF(solution)
        val compatibleAndFreeView: Int => Seq[Boolean] = solutionView.compatibleAndFreeView
        val solutionMatrix: mutable.Map[Int, Int] = solutionView.solMatrix
        def recurse() =
          doAssignments(solution, solutionViewF, canBacktrack, idx + 1)

        if (idx >= m)
          Some(solution)
        else if (solutionMatrix contains idx)
          recurse()
        else {
          val possibleAssignments = compatibleAndFreeView(idx).count(identity)
          if (possibleAssignments == 1) {
            stuffChanged = true
            solutionView(idx) = solutionView.compatibleAndFreeView(idx) indexOf true
            recurse()
          } else if (possibleAssignments == 0) {
            None
          } else if (canBacktrack) {
            //Here we know that we are in the i2j case - idx is just i.
            //sort possibilities by score and remove the ones included inside another. Scores are just the result of merging.
            val possibleAssignmentsIdxs: Seq[Int] = compatibleAndFreeView(idx).zipWithIndex filter (_._1) map (_._2)
            val possibleAssignmentsScores =
              (possibleAssignmentsIdxs map (j => (j, scores_i2j(idx)(j), k2(j))) sortBy (_._2) groupBy
                (_._2) mapValues (sameScoreAssignments =>
              sieveTheWorse(sameScoreAssignments.toList)(ass1 => ass2 => betterString(ass1._3, ass2._3)) map (idx -> _._1) toMap)).toSeq sortBy (_._1)
            println(possibleAssignmentsScores)

            var finalSolution: Option[Solution] = None
            breakable {
              for {
                (score, choices) <- possibleAssignmentsScores
                (idx, assign) <- choices
              } {
                val newSol = solution.dup
                solutionView(idx) = assign
                //Better approach:
                //mainLoop(solution)
                doAssignments(newSol, solutionViewF, canBacktrack, idx + 1) match {
                  case s @ Some(solution) =>
                    finalSolution = s
                    break
                  case None =>
                }
              }
            }
            finalSolution
          } else {
            recurse()
          }
        }
      }

      var currSolution: Option[Solution] = Some(solution)
      while (stuffChanged) {
        stuffChanged = false
        //Order is important here. Try everything possible before backtracking.
        currSolution =
          for {
            sol0 <- currSolution
            sol1 <- doAssignments(sol0, _.j2iview, false, 0)
            sol2 <- doAssignments(sol1, _.i2jview, false, 0)
            sol3 <- if (!stuffChanged)
              doAssignments(sol2, _.i2jview, true, 0)
            else //if stuffChanged, we do another full iteration before backtracking.
              Some(sol2)
          } yield sol3
      }
      currSolution
    }


      /*
      if (doAssignments(solution, _.j2iview, false, 0) ||
        doAssignments(solution, _.i2jview, false, 0))
        return "IMPOSSIBLE"
      if (!stuffChanged && doAssignments(solution, _.i2jview, true, 0))
        return "IMPOSSIBLE"
       */

    mainLoop(solution) match {
      case None =>
        "IMPOSSIBLE"
      case Some(currSolution) =>
        if (currSolution.i2j.size == m) {
          (for {
            i <- 0 until m
            j = solution.i2j(i)
          } yield scores_i2j(i)(j)).mkString
        } else ""
    }
  }
}
