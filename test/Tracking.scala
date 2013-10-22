
import redosignals._
import RedoSignals._
import scalaz.Scalaz._

object Tracking extends App {
  {
    implicit val observing = new Observing

    val x = new Source[Int](1)
    val y = new Source[Int](2)
    val z = ((x: Target[Int]) |@| y)(_ + _)
    val w = ((x: Target[Int]) |@| y)(_ - _)

    trackingRepeat { implicit t =>
      println(z.track * w.track)
    }

    delayingUpdates { implicit t =>
      x <<= 2
      y <<= 3
    }
  }
}

