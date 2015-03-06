
import javax.swing.SwingUtilities

import redosignals.Observing
import redosignals.RedoSignals._
import scala.concurrent.duration._

object ClockTest extends App {
  private[this] implicit val redoObserving = new Observing

  {
    SwingUtilities.invokeLater {
      new Runnable {
        override def run(): Unit = {
          val time = clock(100 millis)

          time foreach println
        }
      }
    }
  }
}

