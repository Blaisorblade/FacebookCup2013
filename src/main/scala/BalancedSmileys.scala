import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers

trait BalancedSmileys extends RegexParsers {
  override def skipWhitespace = false
  def validChar: Parser[Elem] = acceptIf {
    case ' ' => true
    case ':' => true
    case x: Char if x.isLower => true
    case _ => false
  }(_ => "")

  lazy val msg: Parser[String] =
    rep("[a-z :]".r) ^^ (strs => (strs fold "")(_ + _)) | //rep(validChar) |
    ":(" | ":)" |
    '(' ~> msg <~ ')' |
    msg ~ msg ^^ {case a ~ b => a + b}
}
