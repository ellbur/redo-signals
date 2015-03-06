
import reactive.{EventSource, EventStream}
import redosignals.{Observing, Source, Target}

object ArbiterTest extends App with reactive.Observing {
  implicit val redoObserving = new Observing
  
  case class ArbitratedPipe[A, K](active: Target[Boolean], blockedBy: Target[Option[K]], data: EventStream[A])

  class Arbiter[A, K](ev: EventStream[A], activeUser: Target[Option[K]]) {
    def forUser(k: K): ArbitratedPipe[A, K] = {
      val active = activeUser map (_.contains(k))
      ArbitratedPipe(active, activeUser map { case Some(`k`) => None case None => None case Some(o) => Some(o) }, active.window(ev))
    }
  }

  lazy val main = new EventSource[Int]
  lazy val activeUser = new Source[Option[String]](None)
  lazy val arbiter = new Arbiter[Int, String](main, activeUser)
  
  val aPipe = arbiter.forUser("a")
  val bPipe = arbiter.forUser("b")
  
  aPipe.data foreach { x => println(s"A: $x") }
  aPipe.active zip aPipe.blockedBy foreach {
    case (true, _) => println("A active")
    case (false, Some(o)) => println(s"A blocked by $o")
    case (false, None) => println(s"A silent but no one else using it")
  }

  bPipe.data foreach { x => println(s"B: $x") }
  bPipe.active zip bPipe.blockedBy foreach {
    case (true, _) => println("B active")
    case (false, Some(o)) => println(s"B blocked by $o")
    case (false, None) => println(s"B silent but no one else using it")
  }

  main.fire(1)
  activeUser() = Some("a")
  main.fire(2)
  activeUser() = Some("b")
  main.fire(3)
}
