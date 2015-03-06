
import redosignals.{Observing, Source}
import redosignals.RedoSignals._
import math._
import concurrent.duration._

object CoalesceSimilarTest extends App {
  private[this] implicit val redoObserving = new Observing

  {
    val base = new Source[Int](0)
    val throttled = coalesceSimilar(base) { (a, b) =>
      if (abs(a - b) <= 5)
        SimilarFor(1 second)
      else
        Dissimilar
    }

    throttled foreach println

    def step(n: Int) {
      if (n < 100) {
        base() = n
        Thread.sleep(50)
        step(n + 1)
      }
    }
    step(0)
  }
}
