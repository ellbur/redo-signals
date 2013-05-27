
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
}
