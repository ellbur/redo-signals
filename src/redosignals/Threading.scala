
package redosignals

import javax.swing.SwingUtilities

import scala.actors.Actor
import scala.actors.Actor._

trait Threading { self: RedoSignals.type =>
  class SlowTarget[A,B](from: Target[A], to: A => B) extends ComputedTarget[Option[B]] {
    private[this] implicit val redoObserving = new Observing

    override def finalize(): Unit = {
      super.finalize()
    }

    override protected def compute() = source.rely(upset)

    private lazy val source = new Source[Option[B]](None)

    from foreach { next =>
      writerModerator ! Write(next)
    }

    def dispose() {
      writer ! Dispose
      writerModerator ! Dispose
    }

    private case class Write(data: A)
    case object ReadyForData
    private case object Dispose

    private lazy val writerModerator: Actor = actor {
      def pending(data: A) {
        react {
          case Dispose =>
          case ReadyForData =>
            writer ! Write(data)
            noPendingNotReady()
          case Write(data) =>
            pending(data)
        }
      }

      def noPendingNotReady() {
        react {
          case Dispose =>
          case Write(data) =>
            pending(data)
          case ReadyForData =>
            noPendingReady()
        }
      }

      def noPendingReady() {
        react {
          case Dispose =>
          case write: Write =>
            writer ! write
            noPendingNotReady()
          case ReadyForData =>
            noPendingReady()
        }
      }

      noPendingReady()
    }

    private lazy val writer: Actor = actor {
      def main() {
        react {
          case Write(input) =>
            try {
              val output = to(input)
              later {
                source() = Some(output)
              }
            }
            catch {
              case t: Throwable =>
                t.printStackTrace()
            }
            finally {
              writerModerator ! ReadyForData
              main()
            }

          case Dispose =>
            // Allow the actor to terminate

          case _ =>
            main()
        }
      }

      main()
    }
  }

  def computeOnSeparateThread[A,B](from: Target[A])(to: A => B): Target[Option[B]] =
    new SlowTarget[A,B](from, to)

  def onEDT[A](f: => A): A = {
    if (SwingUtilities.isEventDispatchThread) {
      f
    }
    else {
      var theThing: Option[A] = None

      SwingUtilities.invokeAndWait(new Runnable {
        override def run(): Unit = {
          theThing = Some(f)
        }
      })

      theThing.get
    }
  }

  class SynchronizedValue[A](from: Target[A]) {
    implicit val redoObserving = new Observing

    private var it: A = onEDT(from.now)

    def apply() = synchronized(it)
    def get = apply()

    later {
      from foreach (x => synchronized(it = x))
    }
  }

  def synchronize[A](from: Target[A]): SynchronizedValue[A] = new SynchronizedValue(from)
}
