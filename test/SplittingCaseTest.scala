
import redosignals.{Observing, Source}

object SplittingCaseTest extends App {
  private[this] implicit val redoObserving = new Observing

  val x = new Source[Either[Int,String]](Left(1))

  val y = x.split left { a =>
    Left(a)
  } right { b =>
    Right(b)
  }

  y foreach {
    case Left(a) =>
      println("== Left ==")
      a foreach { a =>
        println(a)
      }

    case Right(b) =>
      println("== Right ==")
      b foreach { b =>
        println(b)
      }
  }

  x() = Left(2)
  x() = Right("Bob")
  x() = Right("Nancy")
  x() = Left(3)
}
