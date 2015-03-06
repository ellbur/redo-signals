
package redosignals

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.Timer
import scala.concurrent.duration.Duration
import math._

trait Timing { self: RedoSignals.type =>
  sealed trait Similarity
  case object Dissimilar extends Similarity
  case class SimilarFor(duration: Duration) extends Similarity

  def coalesceSimilar[A](from: Target[A])(decide: (A, A) => Similarity): Target[A] = new ComputedTarget[A] { self =>
    private var timer: Option[Timer] = None
    private var last: Option[(A, Long)] = None

    override protected def compute(): A = {
      timer foreach (_.stop())
      timer = None

      val next = from.rely(upset)
      last match {
        case None =>
          last = Some((next, System.currentTimeMillis))
          next
        case Some((last, lastTime)) =>
          decide(last, next) match {
            case Dissimilar =>
              self.last = Some((next, System.currentTimeMillis))
              next
            case SimilarFor(duration) =>
              val lateness = (System.currentTimeMillis - lastTime).toInt
              val okTime = duration.toMillis.toInt - lateness
              if (okTime > 0) {
                val newTimer =
                  new Timer(okTime, new ActionListener {
                    override def actionPerformed(e: ActionEvent): Unit = {
                      upset()()
                    }
                  })
                newTimer.setRepeats(false)
                newTimer.start()
                timer = Some(newTimer)
                last
              }
              else {
                self.last = Some((next, System.currentTimeMillis))
                next
              }
          }
      }
    }
  }

  def extendFor(from: Target[Boolean], duration: Duration): Target[Boolean] = new ComputedTarget[Boolean] {
    private var timer: Option[Timer] = None
    private var wentFalseTime: Option[Long] = None

    override protected def compute(): Boolean = {
      timer foreach (_.stop())
      timer = None

      val next = from.rely(upset)

      next match {
        case true =>
          wentFalseTime = None
          true

        case false =>
          val remainingMillis =
            wentFalseTime match {
              case None =>
                wentFalseTime = Some(System.currentTimeMillis)
                Some(duration.toMillis)

              case Some(wentFalseTime) =>
                val remaining = duration.toMillis - (System.currentTimeMillis - wentFalseTime)
                if (remaining > 0)
                  Some(remaining)
                else
                  None
            }

          remainingMillis match {
            case None =>
              false

            case Some(remainingMillis) =>
              val newTimer =
                new Timer(remainingMillis.toInt, new ActionListener {
                  override def actionPerformed(e: ActionEvent): Unit = {
                    upset()()
                  }
                })
              newTimer.setRepeats(false)
              newTimer.start()
              timer = Some(newTimer)

              true
          }
      }
    }
  }

  def rampDown(from: Target[Double], fallRatePerS: Double, samplingPeriod: Duration): Target[Double] = new ComputedTarget[Double] {
    private var timer: Option[Timer] = None

    private var peg: Option[(Long, Double, Double)] = None

    override protected def compute(): Double = {
      timer foreach (_.stop())
      timer = None

      val nextValue = from.rely(upset)
      val nextTime = System.currentTimeMillis()

      peg match {
        case None =>
          peg = Some((nextTime, nextValue, nextValue))
          nextValue

        case Some((lastTime, lastDesiredValue, lastActualValue)) =>
          val timeDelta = (nextTime - lastTime) / 1e3

          val minimumAllowedValue = max(lastDesiredValue, lastActualValue - fallRatePerS*timeDelta)

          if (nextValue < minimumAllowedValue) {
            val compromiseValue = minimumAllowedValue

            val newTimer =
              new Timer(samplingPeriod.toMillis.toInt, new ActionListener {
                override def actionPerformed(e: ActionEvent): Unit = {
                  upset()()
                }
              })
            newTimer.setRepeats(false)
            newTimer.start()
            timer = Some(newTimer)

            peg = Some((nextTime, nextValue, compromiseValue))

            compromiseValue
          }
          else {
            nextValue
          }
      }
    }
  }
}
