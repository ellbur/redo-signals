
package redosignals

import scalaz.{Monad, Applicative}

trait ScalazSupport {
  implicit object SignalMonad extends Monad[Target] {
    def pure[A](x: => A) = new Pure[A](x)
    override def apply[A,B](f: Target[A => B], x: Target[A]): Target[B] =
      (f zip x) map {
        case (f, x) => f(x)
      }
    override def bind[A,B](x: Target[A], f: A => Target[B]): Target[B] = x flatMap f
  }
}
