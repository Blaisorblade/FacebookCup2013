package round1
import util._
import com.vividsolutions.jts.index.strtree.STRtree
import com.vividsolutions.jts.geom.Envelope
import scala.collection.mutable
import mutable.ArrayBuffer

object Prob3 extends Logging with CmdlineInput with Timing {
  def split(str: String) = str split ' '
  def toInts(str: String) = split(str) map (Integer parseInt _)

  /*
   * 1 ≤ T ≤ 20
   * 1 ≤ W, H ≤ 40 000
   * 1 ≤ P ≤ W
   * 1 ≤ Q ≤ H
   * 1 ≤ N ≤ min(1 000 000, W * H)
   * 1 ≤ a, b, c, d ≤ 100
   * 0 ≤ X < W
   * 0 ≤ Y < H
   */
  case class Params(w: Int, h: Int, p: Int, q: Int, n: Int, x: Int, y: Int, a: Int, b: Int, c: Int, d: Int)
  case class Point(x: Int, y: Int)

  //x: Int, y: Int, a: Int, b: Int, c: Int, d: Int, w: Int, h: Int, n: Int
  def fillArray(params: Params): ArrayBuffer[Point] = {
    import params._
    def next(p: Point): Point = Point((p.x * a + p.y * b + 1) % w, (p.x * c + p.y * d + 1) % h)
    ArrayBuffer.iterate(Point(x, y), n)(next)
  }

  val eps = 0.01
  //v for vertex.
  //Note that we convert from pixels (squares of size 1) to real size-0 points! The point is the top-left point of the pixel.
  def pixel2DeadAreaEnvelope(v: Point, params: Params): Envelope = {
    import params._
    new Envelope(v.x - p + 1 + eps, v.x + 1 - eps, v.y - q + 1 + eps, v.y + 1 - eps)
  }
  def pixel2Envelope(p: Point): Envelope = new Envelope(p.x, p.x + 1, p.y, p.y + 1)

  def pixel2PictureEnvelope(v: Point, params: Params): Envelope = {
    import params._
    new Envelope(v.x + eps, v.x + p - eps, v.y + eps, v.y + q - eps)
  }

  def main(args: Array[String]) {
    val (lines, m) = getInputAndCount(args)
    //We should get the same solution both ways.
    val bigDeadRectangles = args(1).toBoolean

    processInput(lines, m) { line =>
      timed {
      val nk = toInts(line)
      val params = Params(nk(0), nk(1), nk(2), nk(3), nk(4), nk(5),
          nk(6), nk(7), nk(8), nk(9), nk(10))
      val tree = new STRtree()
      val deadPixels = fillArray(params)
      println(deadPixels)
      for (p <- deadPixels) {
        val env =
          if (bigDeadRectangles)
            pixel2DeadAreaEnvelope(p, params)
          else
            pixel2Envelope(p)
        //println(env)
        tree.insert(env, null)//(env, p))
      }
      var count = 0
      for {
        x <- 0 to (params.w - params.p)
        y <- 0 to (params.h - params.q)
      } {
        val queryEnv =
          if (bigDeadRectangles)
            pixel2Envelope(Point(x, y))
          else
            pixel2PictureEnvelope(Point(x, y), params)
        if (tree.query(queryEnv).size() == 0) {
          count += 1
          println(Point(x, y))
        }
      }
      s"${count}"
      }
    }
  }
}
