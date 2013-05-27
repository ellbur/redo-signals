
package redosignals

import scala.collection.mutable
import scalaz._
import Scalaz._
import reactive.EventSource

trait Target[+A] {
  def rely(changed: () => (() => Unit)): A

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
}

trait Observing {
  private val observed = mutable.ListBuffer[AnyRef]()
  def observe(x: AnyRef) {
    observed += x
  }

  implicit val redoObserving = this
}

class Source[A](init: A) extends Target[A] {
  private var current: A = init
  private val listeners = mutable.ListBuffer[() => (() => Unit)]()
  def update(next: A) {
    if (! (next eq current))
      changed.fire(next)
    current = next
    val toUpdate = listeners.toSeq
    listeners.clear()
    val followUps = toUpdate map (_())
    followUps foreach (_())
  }
  val changed = new EventSource[A]

  def rely(changed: () => (() => Unit)) = {
    listeners += changed
    current
  }
}

class Pairing[A,B](sigA: Target[A], sigB: Target[B]) extends Target[(A, B)] {
  private var current: Option[(A, B)] = None
  private var listeners = mutable.ListBuffer[() => () => Unit]()
  def rely(changed: () => () => Unit): (A, B) = {
    listeners += changed
    current match {
      case Some(x) => x
      case None =>
        val a = sigA.rely(upset _)
        val b = sigB.rely(upset _)
        current = Some((a, b))
        (a, b)
    }
  }
  private def upset(): () => Unit = {
    if (current != None) {
      current = None
      val toUpdate = listeners.toSeq
      listeners.clear()
      val followUps = toUpdate map (_())

      { () =>
        followUps foreach (_())
      }
    }
    else { () => () }
  }
}

class Mapping[A,B](sig: Target[A], f: A => B) extends Target[B] {
  private var current: Option[B] = None
  private var listeners = mutable.ListBuffer[() => () => Unit]()

  def rely(changed: () => () => Unit) = {
    listeners += changed
    current match {
      case Some(x) => x
      case None =>
        val x = f(sig.rely(upset _))
        current = Some(x)
        x
    }
  }
  private def upset(): () => Unit = {
    if (current != None) {
      current = None
      val toUpdate = listeners.toSeq
      listeners.clear()
      val followUps = toUpdate map (_())

      { () =>
        followUps foreach (_())
      }
    }
    else { () => () }
  }
}

class Pure[A](a: A) extends Target[A] {
  def rely(f: () => () => Unit) = a
}

class Switch[A](sig: Target[Target[A]]) extends Target[A] {
  private var current: Option[A] = None
  private var listeners = mutable.ListBuffer[() => () => Unit]()
  def rely(changed: () => () => Unit): A = {
    listeners += changed
    current match {
      case Some(x) => x
      case None =>
        val x = sig.rely(upset).rely(upset)
        current = Some(x)
        x
    }
  }
  private def upset(): () => Unit = {
    current = None
    val toUpdate = listeners.toSeq
    listeners.clear()
    val followUps = toUpdate map (_())

    { () =>
      followUps foreach (_())
    }
  }
}
