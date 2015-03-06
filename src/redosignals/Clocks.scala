
package redosignals

import java.awt.event.{ActionEvent, ActionListener}
import java.util.Date
import javax.swing.Timer

import scala.concurrent.duration.Duration

trait Clocks { this: RedoSignals.type =>
  def clock(resolution: Duration): Target[Date] = new ComputedTarget[Date] {
    override protected def compute(): Date = {
      val timer = new Timer(resolution.toMillis.toInt, new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = {
          upset()()
        }
      })
      timer.setRepeats(false)
      timer.start()

      new Date
    }
  }
}
