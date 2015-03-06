
package redosignals

import collection.mutable

trait EventBlock[+T] {
  def block: T
}

class EventBlockSource[T] extends EventBlock[T] { self =>
  def fire(x: T): Unit = {
    self.synchronized {
      val toNotify = listeners.toList
      listeners.clear()
      toNotify foreach (_(x))
    }
  }

  override def block: T = {
    self.synchronized {
      var it: Option[T] = None

      listeners += { gotIt =>
        self.synchronized {
          it = Some(gotIt)
          self.notify()
        }
      }

      def spin(): T = {
        it match {
          case None =>
            self.wait()
            spin()
          case Some(geez) =>
            geez
        }
      }
      spin()
    }
  }

  private val listeners = mutable.ArrayBuffer[T => Unit]()
}
