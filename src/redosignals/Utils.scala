
package redosignals

import scala.ref.WeakReference

trait Utils { self: RedoSignals.type =>
  def loopOn[A](sig: Target[A])(f: A => Unit)(implicit obs: Observing) {
    obs.observe(f)
    loopOnWeak(sig)(WeakReference(f))
  }

  def loopOnWeak[A](sig: Target[A])(f: WeakReference[A => Unit]) {
    def go() {
      f.get foreach { f =>
        f(sig.rely(changed))
      }
    }
    lazy val changed = () => () => {
      go()
    }
    go()
  }

  def tracking[A](f: Tracker => A): Target[A] = new TargetTracker[A](f)

  def trackingRepeat(f: Tracker => Unit)(implicit obs: Observing) {
    obs.observe(f)
    trackingRepeatWeak(WeakReference(f))
  }

  def trackingRepeatWeak(f: WeakReference[Tracker => Unit]) {
    object tracker extends ActingTracker {
      def run() {
        f.get foreach (_(tracker))
      }
    }
    tracker.run()
  }

  def trackingFor(update: => Unit)(f: Tracker => Unit)(implicit obs: Observing) {
    val updater = () => update
    obs.observe(updater)
    trackingForWeak(WeakReference(updater))(f)
  }

  def trackingForWeak(updater: WeakReference[() => Unit])(f: Tracker => Unit) {
    object tracker extends ActingTracker {
      def run() {
        updater.get foreach (_())
      }
    }
    f(tracker)
  }

  def delayingUpdates(f: UpdateSink => Unit) {
    val sink = new UpdateSink
    f(sink)
    sink()
  }
}
