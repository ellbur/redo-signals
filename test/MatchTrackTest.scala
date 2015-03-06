
import redosignals.{Observing, TargetMutability, Mutability}

object MatchTrackTest extends App {
  implicit val redoObserving = new Observing

  trait Yo {
    val mutability: Mutability
    import mutability._

    trait RNode {
      def toNode: M[Node]
    }

    sealed trait Node
    case object Foo extends Node
    case object Bar extends Node

    object N {
      def unapply(r: RNode): Option[M[Node]] = Some(r.toNode)
    }

    object T {
      def unapply[A](n: M[A])(implicit t: Tracker): Option[A] = Some(n.track)
    }
  }

  object TargetYo extends Yo {
    val mutability: TargetMutability.type = TargetMutability
  }

  {
    import TargetYo._
    import mutability._

    val base = new RNode {
      val toNode = S[Node](Foo)
    }

    val baseIsFoo = tracking { implicit t =>
      base match {
        case N(T(Foo)) => true
        case _ => false
      }
    }

    baseIsFoo foreach println

    base.toNode() = Bar
    base.toNode() = Foo
  }
}
