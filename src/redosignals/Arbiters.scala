
package redosignals

import reactive.EventStream

trait Arbiters { self: RedoSignals.type =>
  case class ArbitratedPipe[A, K](active: Target[Boolean], blockedBy: Target[Option[K]], data: EventStream[A])

  class Arbiter[A, K](ev: EventStream[A], activeUser: Target[Option[K]]) {
    def forUser(k: K): ArbitratedPipe[A, K] = {
      val active = activeUser map (_.contains(k))
      ArbitratedPipe(active, activeUser map { case Some(`k`) => None case None => None case Some(o) => Some(o) }, active.window(ev))
    }
  }
}
