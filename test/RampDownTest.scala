
import redosignals.{Observing, Source}
import concurrent.duration._
import scala.language.postfixOps
import redosignals.RedoSignals._

object RampDownTest extends App {
  implicit val redoObserving = new Observing

  later {
    val x = new Source[Double](0.0)
    val y = x.rampDown(1/0.5, 0.05 second)

    y foreach println

    x() = 1.0
    x() = 0.0
  }
}
