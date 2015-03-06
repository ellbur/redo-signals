
import reactive.{EventSource, EventStream}
import redosignals.Source

object WindowTest extends App with reactive.Observing {
  {
    val w = new Source[Boolean](false)
    val e = new EventSource[Int]

    w.window(e) foreach println

    e.fire(1)
    w() = true
    e.fire(2)
    w() = false
    e.fire(3)
  }
}
