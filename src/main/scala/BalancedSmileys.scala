import language.postfixOps
import com.codecommit.gll._
//import com.codecommit.util

trait Timing {
  this: Logging =>
  def timed[T](toTime: => T): T = {
    val start = System.nanoTime()
    val res = toTime
    val end = System.nanoTime
    println(s"${(end - start)/(1000 * 1000)} ms needed")
    res
  }
}

trait BalancedSmileys extends RegexParsers {
  override final val skipWhitespace = false

  val charRegex: Parser[String] = "[a-z ]".r
  //Avoid concatenating strings which we don't use.
  def foldStrs(strs: List[String]): String = "" //(strs fold "")(_ + _) 
  //XXX: Making the rep1 a rep makes this grammar indirectly left-recursive, which is bad. Unlike declared, it doesn't seem to work.
  lazy val msg: Parser[String] =
    {
      {
        rep1(charRegex) ^^ foldStrs |
          ":" <~ ("[()]".r ?) | //XXX: Note that opt("(" | ")") in place of ("[()]".r ?) does not work.
          "(" ~> msg <~ ")"
      } <~ opt(msg)
    } | ""
}

object BalancedSmileysDriver extends BalancedSmileys with CmdlineInput with Logging with Timing {
  def main(args: Array[String]) {
    val (lines, t) = getInputAndCount(args) //t is at most 50.
    processInput(lines, t) { line =>
      timed {
        if (msg(line) collectFirst {
          case x @ Success(y, rest) if rest.isEmpty => ()
        } isEmpty) {
          "NO"
        } else {
          "YES"
        }
      }
    }
  }
}
