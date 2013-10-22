
package redosignals

import scala.collection.mutable
import reactive.EventSource
import scala.collection.mutable.ArrayBuffer
import scalaz.{Name, Need}

trait Target[+A] {
  def track(implicit tracker: Tracker): A = tracker.track(this)

  def rely(changed: () => () => Unit): A

  def zip[B](other: Target[B]): Target[(A, B)] =
    new Pairing[A,B](this, other)
  def map[B](f: A => B): Target[B] =
    new Mapping[A,B](this, f)
  def flatMap[B](f: A => Target[B]): Target[B] =
    new Switch(this map f)

  def now: A = rely(() => () => ())

  def foreach(f: A => Unit)(implicit obs: Observing) {
    RedoSignals.loopOn(this)(f)
  }

  def apply(f: A => Unit)(implicit obs: Observing) {
    foreach(f)(obs)
  }
}

trait Tracker {
  def track[A](t: Target[A]): A
}

trait ActingTracker extends Tracker {
  def track[A](t: Target[A]): A = {
    t.rely { () =>
      invalidate()
      () =>
        update()
    }
  }

  var valid: Boolean = false
  def run()
  def update() {
    if (!valid) {
      run()
      valid = true
    }
  }
  def invalidate() {
    valid = false
  }
}

class TargetTracker[A](f: Tracker => A) extends Tracker with ComputedTarget[A] {
  def track[B](t: Target[B]): B = t.rely(upset)
  protected def compute(): A = f(this)
}

class UpdateSink {
  val deferred = ArrayBuffer[() => Unit]()
  def defer(g: () => Unit) { deferred += g }
  def apply() { deferred foreach (_()) }
}

class Observing {
  private val observed = mutable.ListBuffer[AnyRef]()
  def observe(x: AnyRef) {
    observed += x
  }

  implicit val redoObserving = this
}

class Source[A](init: => A) extends Target[A] {
  private var current: Name[A] = Need(init)
  private val listeners = mutable.ListBuffer[() => () => Unit]()
  def update(next: A) {
    if (! (next == current.value))
      changed.fire(next)
    val toUpdate = synchronized {
      current = Need(next)
      val t = listeners.toSeq
      listeners.clear()
      t
    }
    val followUps = toUpdate map (_())
    followUps foreach (_())
  }
  def <<=(next: A)(implicit sink: UpdateSink) {
    if (! (next == current.value))
      changed.fire(next)
    val toUpdate = synchronized {
      current = Need(next)
      val t = listeners.toSeq
      listeners.clear()
      t
    }
    val followUps = toUpdate map (_())
    sink.defer {
      () => {
        followUps foreach (_())
      }
    }
  }
  val changed = new EventSource[A]

  def rely(changed: () => (() => Unit)) = {
    listeners += changed
    current.value
  }
}

trait ComputedTarget[A] extends Target[A] {
  private var current: Option[A] = None
  private var listeners = mutable.ListBuffer[() => () => Unit]()

  protected def compute(): A

  def rely(changed: () => () => Unit): A = {
    val it = synchronized {
      listeners += changed
      current
    }
    it match {
      case Some(x) => x
      case None =>
        val computed = compute()
        synchronized {
          current = Some(computed)
        }
        computed
    }
  }

  protected def upset(): () => Unit = {
    val toNotify = synchronized {
      if (current.isDefined) {
        current = None
        val t = listeners.toSeq
        listeners.clear()
        Some(t)
      }
      else None
    }
    toNotify match {
      case None => { () => () }
      case Some(toUpdate) =>
        val followUps = toUpdate map (_())

      { () =>
        followUps foreach (_())
      }
    }
  }
}

class Pairing[A,B](sigA: Target[A], sigB: Target[B]) extends ComputedTarget[(A, B)] {
  def compute() =
    (sigA.rely(upset), sigB.rely(upset))
}

class Mapping[A,B](sig: Target[A], f: A => B) extends ComputedTarget[B] {
  def compute() =
    f(sig.rely(upset))
}

class Pure[A](a: A) extends Target[A] {
  def rely(f: () => () => Unit) = a
}

class Switch[A](sig: Target[Target[A]]) extends ComputedTarget[A] {
  def compute() =
    sig.rely(upset).rely(upset)
}
