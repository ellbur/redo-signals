
object Watching {
  import scalaz._
  import Scalaz._
  import redosignals._
  import RedoSignals._

  def main(args: Array[String]) {
    implicit val obs = new Observing { }

    val a = new Source[Int](0)
    val x = a map (_ * 2)
    val y = a map (_ * 3)
    val z = ((x: Target[Int]) |@| (y: Target[Int])) { (x, y) =>
      println("Calculating z")
      x + y
    }

    z foreach (println(_))

    a() = 3
    a() = 4
  }
}
