
package redosignals

import scala.language.implicitConversions
import scala.ref.WeakReference

trait Mutability { module =>
  type M[+X] <: TargetLike[X]
  type S[X] <: M[X]

  def tracking[A](f: Tracker => A): M[A]

  trait TargetLike[+A] {
    val mutability: module.type = module
    def track(implicit tracker: Tracker): A
    def trackDebug(name: String)(implicit tracker: Tracker): A
    def zip[B](other: M[B]): M[(A, B)]
    def map[B](f: A => B): M[B]
    def flatMap[B](f: A => M[B]): M[B]
    def biFlatMap[B](f: A => S[B]): S[B]
    def now: A
    def foreach(f: A => Unit)(implicit obs: Observing)
    def apply(f: A => Unit)(implicit obs: Observing)
    def immediatelyCheckingChanged: M[A]
  }

  trait Tracker {
    def track[A](t: M[A]): A
    def track[A](t: M[A], name: String): A
  }

  def trackingRepeat(f: Tracker => Unit)(implicit obs: Observing)
  def trackingFor(update: => Unit)(f: Tracker => Unit)(implicit obs: Observing)

  def S[T](x: T): S[T]

  def constant[A](x: A): M[A]

  implicit class Constant[A](a: A) {
    def constant: M[A] = module.constant(a)
  }
}

object TargetMutability extends Mutability {
  type M[+X] = Target[X]
  type S[X] = BiTarget[X]

  def tracking[A](f: Tracker => A): M[A] = new TargetTracker[A](f)

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

  override def S[T](x: T): S[T] = new Source[T](x)

  override def constant[A](x: A): M[A] = new Pure(x)

  object Track {
    def unapply[A](n: M[A])(implicit t: Tracker): Option[A] = Some(n.track)
  }
}

object IdentityMutability extends Mutability {
  case class Identity[+A](value: A) extends TargetLike[A] {
    override def track(implicit tracker: Tracker): A = value
    override def flatMap[B](f: (A) => M[B]): M[B] = f(value)
    override def now: A = value
    override def trackDebug(name: String)(implicit tracker: Tracker): A = value
    override def zip[B](other: M[B]): M[(A, B)] = Identity((value, other.value))
    override def apply(f: (A) => Unit)(implicit obs: Observing): Unit = f(value)
    override def foreach(f: (A) => Unit)(implicit obs: Observing): Unit = f(value)
    override def map[B](f: (A) => B): M[B] = Identity(f(value))
    override def immediatelyCheckingChanged: M[A] = this
    override def biFlatMap[B](f: (A) => S[B]): S[B] = f(value)
  }
  object Identity {
    implicit def extract[T](x: Identity[T]): T = x.value
  }

  type M[+X] = Identity[X]
  type S[X] = Identity[X]

  val tracker = new Tracker {
    override def track[B](t: M[B]): B = t.value
    override def track[B](t: M[B], name: String): B = t.value
  }

  override def tracking[A](f: (Tracker) => A): M[A] = Identity(f(tracker))

  override def trackingRepeat(f: (IdentityMutability.Tracker) => Unit)(implicit obs: Observing): Unit = f(tracker)
  override def trackingFor(update: => Unit)(f: (IdentityMutability.Tracker) => Unit)(implicit obs: Observing): Unit = update

  override def S[T](x: T): S[T] = Identity(x)

  override def constant[A](x: A): M[A] = Identity(x)
}
