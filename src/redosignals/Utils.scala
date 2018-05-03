
package redosignals

import javax.swing.SwingUtilities

import scala.ref.WeakReference

trait Utils { self: RedoSignals.type =>
  implicit class SetLike[-A](f: A => Any) {
    def like(as: Target[A])(implicit obs: Observing): Unit =
      as foreach (x => f(x))
  }

  // Has trouble with type inference. Weird.
  implicit class SetTo[+A](t: Target[A]) {
    def ->(sink: A => Any)(implicit obs: Observing): Unit =
      t foreach (x => sink(x))
  }
  
  def loopOn[A](sig: Target[A])(f: A => Unit)(implicit obs: Observing) {
    obs.observe(f)
    obs.observe(sig)
    loopOnWeak(sig)(WeakReference(f))
  }

  def loopOnWeak[A](sig: Target[A])(f: WeakReference[A => Unit]) {
    var current: Option[A] = None
    def go() {
      f.get foreach { f =>
        val next = sig.rely(changed)
        if (!current.contains(next)) {
          current = Some(next)
          f(next)
        }
      }
    }
    lazy val changed = () => () => {
      go()
    }
    go()
  }

  def loopOnDebug[A](sig: Target[A])(name: String)(f: A => Unit)(implicit obs: Observing) {
    obs.observe(f)
    obs.observe(sig)
    loopOnWeakDebug(sig)(name)(WeakReference(f))
  }

  private var numberCounter: Int = 0

  def loopOnWeakDebug[A](sig: Target[A])(name: String)(f: WeakReference[A => Unit]) {
    val number = numberCounter
    numberCounter += 1
    println(s"Starting loop $name $number")
    var current: Option[A] = None
    def go() {
      println(s"$name $number go()")
      f.get match {
        case Some(f) =>
          println(s"$name $number present")
          val next = sig.rely(changed)
          println(s"$name $number got next $next vs current $current")
          if (!current.contains(next)) {
            println(s"Decided to update it")
            current = Some(next)
            f(next)
          }
        case None =>
          println(s"$name $number absent")
      }
    }
    lazy val changed = () => () => {
      println(s"$name $number changed()")
      go()
    }
    go()
  }

  def delayingUpdates(f: UpdateSink => Unit) {
    val sink = new UpdateSink
    f(sink)
    sink()
  }

  implicit class LastValid[A](t: Target[Option[A]]) {
    def lastValid(init: A): Target[A] = {
      var now: A = init
      t map {
        case Some(x) =>
          now = x
          x
        case None =>
          now
      }
    }
  }

  import TargetMutability.Tracker

  def tracking[A](f: Tracker => A): Target[A] = TargetMutability.tracking(f)

  def trackingRepeat(f: Tracker => Unit)(implicit obs: Observing) = TargetMutability.trackingRepeat(f)(obs)

  def trackingFor(update: => Unit)(f: Tracker => Unit)(implicit obs: Observing) = TargetMutability.trackingFor(update)(f)(obs)

  def immediatelyCheckingChanged[A](sig: Target[A]): Target[A] = sig.immediatelyCheckingChanged

  def later(f: => Unit): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = { f }
    })
  }

  implicit class TargetConstant[A](a: A) {
    def constant: Target[A] = TargetMutability.constant(a)
  }
}
