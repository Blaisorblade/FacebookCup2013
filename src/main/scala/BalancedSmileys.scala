import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

trait BalancedSmileys extends RegexParsers {
  override def skipWhitespace = false
  /*def validChar: Parser[Elem] = acceptIf {
    case ' ' => true
    case ':' => true
    case x: Char if x.isLower => true
    case _ => false
  }(_ => "")*/

  def msg: Parser[String] =
    rep("[a-z ]".r) ^^ (strs => (strs fold "")(_ + _)) | //rep(validChar) |
    ":" <~ opt("(" | ")") |
    "(" ~> msg <~ ")" |
    msg ~ msg ^^ {case a ~ b => a + b}
}

object BalancedSmileysDriver extends BalancedSmileys with CmdlineInput with Logging {
  def main(args: Array[String]) {
    val (lines, t) = getInputAndCount(args) //t is at most 50.
    processInput(lines, t) { line =>
      parse(msg, line) match {
        case Success(x, rest) =>
          val src = rest.source
          val parsed = src.subSequence(rest.offset, src.length)
          println(s"Of line '${line}' only '${parsed}' left")
          "YES"
        case failure : NoSuccess =>
          println(failure.msg)
          "NO"
      }
    }
  }
}
