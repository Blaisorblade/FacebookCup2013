import language.postfixOps
import com.codecommit.gll._
//import com.codecommit.util

trait BalancedSmileys extends RegexParsers {
  override final val skipWhitespace = false
  /*def validChar: Parser[Elem] = acceptIf {
    case ' ' => true
    case ':' => true
    case x: Char if x.isLower => true
    case _ => false
  }(_ => "")*/

  val charRegex: Parser[String] = "[a-z ]".r
  def foldStrs(strs: List[String]): String = (strs fold "")(_ + _) 
  //Making the rep1 a rep makes this grammar indirectly left-recursive, which is bad.
  lazy val msg: Parser[String] =
    {
      {
        rep1(charRegex) ^^ foldStrs |
          ":" <~ opt("(" | ")") |
          "(" ~> msg <~ ")"
      } <~ opt(msg)
    } | ""

    /*
    //Much slower:
    rep((charRegex+) ^^ foldStrs |
    ":" <~ opt("(" | ")") |
    "(" ~> msg <~ ")") ^^ foldStrs
    */

    /*rep("[a-z ]".r) ^^ (strs => (strs fold "")(_ + _)) | //rep(validChar) |
    ":" <~ opt("(" | ")") |
    "(" ~> msg <~ ")" |
    msg ~ msg ^^ (_ + _)*/
}

object BalancedSmileysDriver extends BalancedSmileys with CmdlineInput with Logging {
  def main(args: Array[String]) {
    val (lines, t) = getInputAndCount(args) //t is at most 50.
    processInput(lines, t) { line =>
      if (msg(line) collectFirst {
        case x @ Success(y, rest) if rest.isEmpty => ()
      } isEmpty) {
        for (i <- msg(line)) {
          i match {
            case failure : Failure =>
              println(failure)
            case _ =>
          }
        }
        "NO"
      } else {
        "YES"
      }
      /*msg(line).head match {
        case Success(x, rest) =>
          /*val src = rest.source
          val parsed = src.subSequence(rest.offset, src.length)
          println(s"Of line '${line}' only '${parsed}' left")*/
          if (rest.isEmpty)
            "YES"
          else {
            println(rest)
            "NO"
          }
        case failure : Failure =>
          println(failure.data)
          "NO"
      }*/
    }
  }
}
