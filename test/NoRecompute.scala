
import redosignals._
import RedoSignals._
import scalaz._
import Scalaz._

object NoRecompute {
  def main(args: Array[String]) {
    val x = new Source[Int](0)
    val y = new Source[Int](0)
    val u = ((x: Target[Int]) |@| (y: Target[Int])) { (x, y) =>
      println("Calculating u")
      x + y
    }
    val v = ((x: Target[Int]) |@| (y: Target[Int])) { (x, y) =>
      println("Calculating v")
      x - y
    }
    val t = (u |@| v) { (u, v) =>
      println("Calculating t")
      u * v
    }
    val s = (u |@| v) { (u, v) =>
      println("Calculating s")
      u + v
    }
    val r = (t |@| s) { (t, s) =>
      println("Calculating r")
      t * s
    }

    def setX() {
      println("Setting x")
      x() = 1
    }
    def setY() {
      println("Setting y")
      y() = 1
    }
    def getR() {
      println("Getting r")
      r.rely(() => () => ())
    }

    setX()
    setY()
    getR()
    getR()
    setX()
    getR()
    getR()
    getR()
    getR()
    setX()
    setX()
  }
}
