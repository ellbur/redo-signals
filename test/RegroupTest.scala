import redosignals.{Source, Observing}

object RegroupTest extends App {
  private[this] implicit val redoObserving = new Observing

  val x = new Source[Int](0)

  val y = x.regroup(_ / 10) { (key, thing) =>
    (key, thing)
  }

  y foreach { case (key, x) =>
    println(s"== ${key * 10}s ==")
    x foreach println
  }

  x() = 5
  x() = 2
  x() = 55
  x() = 51
}
