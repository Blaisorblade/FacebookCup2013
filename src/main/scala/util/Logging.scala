package util

trait Logging {
  val debug = true
  def println(msg: Any) = if (debug) Console.err.println(msg)
}
