
package redosignals

import reactive.{EventSource, EventStream}
import redosignals.TargetMutability.M

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.ref.WeakReference

trait Target[+A] extends TargetMutability.TargetLike[A] {
  thisTarget =>
  def track(implicit tracker: TargetMutability.Tracker): A = tracker.track(this)

  def trackDebug(name: String)(implicit tracker: TargetMutability.Tracker): A = tracker.track(this, name)

  def rely(changed: () => () => Unit): A

  def zip[B](other: Target[B]): Target[(A, B)] =
    new Pairing[A, B](this, other)

  def unzip[X, Y](implicit pf: A <:< (X, Y)): (Target[X], Target[Y]) =
    (
      this map (a => pf(a)._1),
      this map (a => pf(a)._2)
    )

  def map[B](f: A => B): Target[B] =
    new Mapping[A, B](this, f)

  def flatMap[B](f: A => Target[B]): Target[B] =
    new Switch(this map f)

  def split[X, Y](implicit isAnEither: A <:< Either[X, Y]) = new {
    def left[C1](left: Target[X] => C1) = new {
      def right[C2 >: C1](right: Target[Y] => C2): Target[C2] =
        new EitherSplit(thisTarget map isAnEither, left, right)
    }
  }

  def regroup[B,C](f: A => B)(g: (B, Target[A]) => C): Target[C] =
    new FunctionSplit(this, f, g)

  def regroupSeq[B,C](f: A => Seq[B])(g: (B, Target[A]) => C): Target[Seq[C]] =
    new SeqFunctionSplit(this, f, g)

  def biFlatMap[B](f: A => BiTarget[B]): BiTarget[B] =
    new BiSwitch(this map f)

  def now: A

  def foreach(f: A => Unit)(implicit obs: Observing) {
    RedoSignals.loopOn(this)(f)
  }

  def foreachDebug(name: String)(f: A => Unit)(implicit obs: Observing): Unit = {
    println(s"foreachDebug($name) { ... }")
    RedoSignals.loopOnDebug(this)(name)(f)
  }

  def apply(f: A => Unit)(implicit obs: Observing) {
    foreach(f)(obs)
  }

  def debugLock(): Unit = {
    rely { () =>
      throw new IllegalStateException("Debug locked signal modified.");
    }
  }

  def immediatelyCheckingChanged: Target[A] = new ImmediatelyCheckingChanged[A](this)

  def zipWithStaleness: Target[(A, A)] = {
    var staleness: A = now
    RedoSignals.tracking { implicit t =>
      val last = staleness
      staleness = track
      (last, staleness)
    }
  }

  def zipWithStalenessFrom[B>:A](default: B): Target[(B, B)] = {
    var staleness: B = default
    RedoSignals.tracking { implicit t =>
      val last = staleness
      staleness = track
      (last, staleness)
    }
  }

  def collect[B](f: PartialFunction[A, B]): Target[Option[B]] = {
    var current: Option[B] = None
    RedoSignals.tracking { implicit t =>
      current = f.lift(track) orElse current
      current
    }
  }

  def window[B](ev: EventStream[B])(implicit pf: A <:< Boolean): EventStream[B] =
    new EventSource[B] with reactive.Observing { resultingStream =>
      implicit val redoObserving = new Observing

      ev foreach { x =>
        if (thisTarget.now)
          resultingStream.fire(x)
      }
    }

  def rampDown(fallRatePerS: Double, samplingPeriod: Duration)(implicit pf: A <:< Double): Target[Double] =
    RedoSignals.rampDown(this map (a => a: Double), fallRatePerS, samplingPeriod)
}

trait ActingTracker extends TargetMutability.Tracker {
  def track[A](t: Target[A]): A = {
    t.rely { () =>
      invalidate()
      () =>
        update()
    }
  }

  def track[A](t: Target[A], name: String) = track(t)

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

class TargetTracker[A](f: TargetMutability.Tracker => A) extends ComputedTarget[A] { self =>
  protected def compute(): A = {
    f(new TargetMutability.Tracker {
      def track[B](t: Target[B]): B = t.rely(self.upset)

      def track[B](t: Target[B], name: String): B =
        t.rely {
          () =>
            println(s"$name triggered")
            self.upset()
        }
    })
  }
}

class UpdateSink {
  val deferred = ArrayBuffer[() => Unit]()

  def defer(g: () => Unit) {
    deferred += g
  }

  def apply() {
    deferred foreach (_())
  }
}

class Observing {
  protected val observed = mutable.ArrayBuffer[AnyRef]()

  def observe(x: AnyRef) {
    observed += x
  }

  implicit val redoObserving = this
}

trait Observer {
  implicit val redoObserving = new Observing
}

class DebugObserving(name: String) extends Observing {
  override def observe(x: AnyRef) {
    println(s"$name observing $x")
    observed += x
  }

  override def finalize(): Unit = {
    println(s"$name finalizing")
    super.finalize()
  }
}

trait CoTarget[-A] extends reactive.Observing { self =>
  lazy val redoObserving = new Observing

  def update(next: A): Unit = {
    delayedUpdate(next)()
  }

  def <<=(next: A)(implicit u: UpdateSink): Unit = {
    u.defer(delayedUpdate(next))
  }

  def <-!-(stream: reactive.EventStream[A]): Unit = {
    stream foreach update
  }

  def delayedUpdate(next: A): () => Unit

  def comap[B](f: B => A): CoTarget[B] = new CoTarget[B] {
    override def delayedUpdate(next: B): () => Unit =
      self.delayedUpdate(f(next))
  }
}

trait BiTarget[A] extends Target[A] with CoTarget[A] { self =>
  def containsBiMap[B](b: B)(implicit ok1: A =:= Set[B], ok2: Set[B] =:= A): BiTarget[Boolean] =
    new ComputedTarget[Boolean] with BiTarget[Boolean] {
      override protected def compute() = self.rely(upset) contains b

      override def delayedUpdate(next: Boolean): () => Unit =
        if (next)
          self.delayedUpdate(self.now + b)
        else
          self.delayedUpdate(self.now - b)
    }

  def ++(implicit num: Numeric[A]) = {
    val r = now
    update(num.plus(r, num.one))
    r
  }
}

class Source[A](init: A) extends BiTarget[A] {
  private var current: A = init
  private val listeners = mutable.ListBuffer[() => () => Unit]()

  def delayedUpdate(next: A): () => Unit = {
    if (!(next == current))
      changed.fire(next)
    val toUpdate = synchronized {
      current = next
      val t = listeners.toSeq
      listeners.clear()
      t
    }
    val followUps = toUpdate map (_())

    () => {
      followUps foreach (_())
    }
  }

  val changed = new reactive.EventSource[A]

  def rely(changed: () => (() => Unit)) = {
    listeners += changed
    current
  }

  override def now: A = current
}

trait ComputedTarget[A] extends Target[A] {
  protected var current: Option[A] = None
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

  def now: A = {
    val it = synchronized {
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

  def upset: () => () => Unit = ComputedTarget.weakUpset(this)

  protected def unsafeUpset(): () => Unit = {
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
      case None => () => ()
      case Some(toUpdate) =>
        val followUps = toUpdate map (_())

      { () =>
        followUps foreach (_())
      }
    }
  }
}

object ComputedTarget {
  def weakUpset[A](c: ComputedTarget[A]): () => () => Unit = 
    weakWeakUpset(WeakReference(c))
  
  def weakWeakUpset[A](c: WeakReference[ComputedTarget[A]]): () => () => Unit = {
    () =>
      c.get map (_.unsafeUpset()) getOrElse (() => ())
  }
}

class Pairing[A, B](sigA: Target[A], sigB: Target[B]) extends ComputedTarget[(A, B)] {
  def compute() =
    (sigA.rely(upset), sigB.rely(upset))
}

class Mapping[A, B](sig: Target[A], f: A => B) extends ComputedTarget[B] {
  def compute() =
    f(sig.rely(upset))
}

class Pure[A](a: A) extends Target[A] {
  def rely(f: () => () => Unit) = a
  override def now: A = a
}

class Switch[A](sig: Target[Target[A]]) extends ComputedTarget[A] {
  def compute() =
    sig.rely(upset).rely(upset)
}

class BiSwitch[A](sig: Target[BiTarget[A]]) extends ComputedTarget[A] with BiTarget[A] {
  def compute() =
    sig.rely(upset).rely(upset)

  override def delayedUpdate(next: A): () => Unit =
    sig.now.delayedUpdate(next)
}

class ImmediatelyCheckingChanged[A](sig: Target[A]) extends Target[A] {
  private var current: A = sig.rely(upset)
  private var listeners = mutable.ListBuffer[() => () => Unit]()

  def rely(changed: () => () => Unit): A = {
    listeners += changed
    current
  }

  protected def upset(): () => Unit = {
    val toNotify = synchronized {
      val next = sig.rely(upset)
      if (next != current) {
        current = next
        val t = listeners.toSeq
        listeners.clear()
        Some(t)
      }
      else None
    }
    toNotify match {
      case None => () => ()
      case Some(toUpdate) =>
        val followUps = toUpdate map (_())

      { () =>
        followUps foreach (_())
      }
    }
  }

  override def now: A = current
}

class EitherSplit[A, B, C](from: Target[Either[A, B]], left: Target[A] => C, right: Target[B] => C) extends ComputedTarget[C] {
  private class StoredLeft(init: A) extends ComputedTarget[A] {
    var last: A = init

    override protected def compute() = {
      from.rely(() => () => ()) match {
        case Left(a) =>
          last = a
          a
        case _ => last
      }
    }
  }

  private class StoredRight(init: B) extends ComputedTarget[B] {
    var last: B = init

    override protected def compute() = {
      from.rely(() => () => ()) match {
        case Right(b) =>
          last = b
          b
        case _ => last
      }
    }
  }

  private var storedTarget: Option[(Either[StoredLeft, StoredRight], C)] = None

  override protected def compute(): C = {
    def fullyUpset(): (() => Unit) = {
      val first = upset()

      val next =
        storedTarget match {
          case Some((Left(t), _)) => t.upset()
          case Some((Right(t), _)) => t.upset()
          case _ => () => ()
        }

      { () =>
        first()
        next()
      }
    }

    from.rely(fullyUpset) match {
      case Left(a) =>
        storedTarget match {
          case Some((Left(storedLeft), c)) => c
          case _ =>
            val storedLeft = new StoredLeft(a)
            val c = left(storedLeft)
            storedTarget = Some((Left(storedLeft), c))
            c
        }
      case Right(b) =>
        storedTarget match {
          case Some((Right(storedRight), c)) => c
          case _ =>
            val storedRight = new StoredRight(b)
            val c = right(storedRight)
            storedTarget = Some((Right(storedRight), c))
            c
        }
    }
  }
}

class FunctionSplit[A,B,C](from: Target[A], f: A => B, g: (B, Target[A]) => C) extends ComputedTarget[C] {
  private case class Stored(key: B) extends ComputedTarget[A] {
    override protected def compute(): A =
      from.rely(() => () => ())
  }

  private var stored: Option[(Stored, C)] = None

  override protected def compute(): C = {
    def fullyUpset(): (() => Unit) = {
      val first = upset()

      val next =
        stored match {
          case Some((t, _)) => t.upset()
          case _ => () => ()
        }

      { () =>
        first()
        next()
      }
    }

    val current = from.rely(fullyUpset)
    val currentKey = f(current)
    stored match {
      case Some((Stored(`currentKey`), c)) => c
      case _ =>
        val nextStored = Stored(currentKey)
        val c = g(currentKey, nextStored)
        stored = Some((nextStored, c))
        c
    }
  }
}

class SeqFunctionSplit[A,B,C](from: Target[A], f: A => Seq[B], g: (B, Target[A]) => C) extends ComputedTarget[Seq[C]] {
  private class Stored(val key: B, _c: => C) extends ComputedTarget[A] {
    lazy val c = _c

    override protected def compute(): A =
      from.rely(() => () => ())
  }

  private var stored: Map[B, Stored] = Map()

  override protected def compute(): Seq[C] = {
    def fullyUpset(): (() => Unit) = {
      val first = upset()

      val next =
        (stored.values map (_.upset())).toSeq

      { () =>
        first()
        next foreach (_())
      }
    }

    val current = from.rely(fullyUpset)
    val currentKeySeq = f(current)

    val nextStoredSeq =
      currentKeySeq map { key =>
        stored.getOrElse(key, {
          lazy val nextStored: Stored = new Stored(key, c)
          lazy val c: C = g(key, nextStored)
          nextStored
        })
      }

    stored = (nextStoredSeq map (s => (s.key, s))).toMap

    nextStoredSeq map (_.c)
  }
}

class MapSource[K, V](default: V) {
  private val holdingMap = mutable.Map[K, V]()
  private val waiting = mutable.Map[K, List[() => () => Unit]]()
  private val holisticWaiting = mutable.ArrayBuffer[() => () => Unit]()

  def apply(key: K): BiTarget[V] = new ComputedTarget[V] with BiTarget[V] {
    override protected def compute(): V = {
      waiting += ((key, (() => upset()) :: waiting.getOrElse(key, Nil)))
      holdingMap.getOrElse(key, default)
    }

    override def delayedUpdate(value: V) = {
      if (key == default)
        holdingMap -= key
      else
        holdingMap += ((key, value))

      val toNotify = waiting.getOrElse(key, Nil) ++ holisticWaiting.toSeq
      waiting -= key
      holisticWaiting.clear()

      val next = toNotify map (_())

      () =>
        next foreach (_())
    }
  }

  def toMap: Target[Map[K, V]] = new ComputedTarget[Map[K, V]] {
    override protected def compute(): Map[K, V] = {
      holisticWaiting += (() => upset())
      holdingMap.toMap
    }
  }

  def update(key: K, value: V): Unit = {
    if (key == default)
      holdingMap -= key
    else
      holdingMap += ((key, value))

    val toNotify = waiting.getOrElse(key, Nil) ++ holisticWaiting.toSeq
    waiting -= key
    holisticWaiting.clear()

    val next = toNotify map (_())
    next foreach (_())
  }
}
