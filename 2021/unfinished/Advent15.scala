import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer
import collection.mutable.PriorityQueue
import scala.util.control.Breaks._
import scala.math.{max, min}

object Advent15 {
  val N = 10

  def main(args: Array[String]): Unit = {
    val walker = new Walker(readInputs())
    println(s"Min risk is ${walker.walk().w}")
  }

  def readInputs(): Array[Array[Int]] = {
    var line = ""
    val field = Array.ofDim[Int](N, N)
    var i = 0
    while ({line = readLine(); line != null}) {
      line.split("").zip(0 until line.size).foreach { case (c, j) => field(i)(j) = c.toInt }
      println(line)
      i += 1;
    }
    return field
  }

  class Walker(val field: Array[Array[Int]]) {
    val pq: PriorityQueue[Path] = new PriorityQueue()(Ordering.by[Path, Int](_.w))

    def walk(): Path = {
      var p: Path = new Path(0, 0, 0, None)
      pq.enqueue(p)
      do {
        p = pq.dequeue()
        if (p.x + p.y == 2*N - 2) { return p }
        childrenOf(p).foreach { pq.enqueue(_) }
        childrenOf(p).foreach { println(_) }
      } while (!pq.isEmpty);
      return p
    }

    def childrenOf(p: Path): ListBuffer[Path] = {
//       -1 to 1 flatMap (i => -1 to 1 map (j => (i, j))).filter { case(i, j) => i >= 0 && j >= 0 && i < N && j < N && !(i == 0 && j == 0) }.map {
//         case (i, j) => (p.x + i, p.y + j)
//       }.map { case(x, y) => p.createChild(x, y, field(x)(y)) }
//     }
      val ret: ListBuffer[Path] = ListBuffer.empty[Path]
      var x: Int = 0
      var y: Int = 0
      for (x <- max(0, p.x - 1) to min(N-1, p.x + 1)) {
        for (y <- max(0, p.y - 1) to min(N-1, p.y + 1)) {
          if (x != 0 || y != 0) ret += p.createChild(x, y, field(x)(y))
        }
      }
      return ret
    }
  }

  class Point(val x: Int, val y: Int)

  class Path(val x: Int, val y: Int, val w: Int, val par: Option[Path]) {
    
    def createChild(x: Int, y: Int, w: Int): Path = new Path(x, y, this.w + w, Some(this))
    override def toString: String = s"($x,$y):$w"
  }
}

