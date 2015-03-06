
import redosignals.{MapSource, Observing}

object MapSourceTest extends App {
  private[this] implicit val observing = new Observing

  val map = new MapSource[String,Int](0)

  map("Bob") foreach { x =>
    println(s"Bob has $x")
  }

  map("Nancy") foreach { x =>
    println(s"Nancy has $x")
  }

  map("Bob") = 1
  map("Bob") = 2
  map("Nancy") = 3
}
