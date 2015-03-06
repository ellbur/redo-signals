
import redosignals.{Target, Source, Observing}

object RegroupSeqTest extends App {
  private[this] implicit val redoObserving = new Observing

  val x = new Source[Seq[Int]](Seq(0, 2, 10, 11))

  var grouperNumber: Int = 0
  def nextGrouperNumber = {
    val i = grouperNumber
    grouperNumber += 1
    i
  }

  class Grouper(val members: Target[Seq[Int]]) {
    val n = nextGrouperNumber
  }

  val y = x.regroupSeq(_ map (_ / 10) distinct) { (key, thing) =>
    (key, new Grouper(thing map (_ filter (_ / 10 == key))))
  }

  y foreach { groups =>
    println(groups map { case (key, grouper) =>
        s"${grouper.n} hold ${key * 10}s"
    } mkString ";  ")

    groups foreach { case (_, grouper) =>
        grouper.members foreach { members =>
          println(s"${grouper.n}: ${members mkString " "}")
        }
    }
  }

  x() = Seq(0, 2, 3, 10)
  x() = Seq(0, 2, 3, 10, 20)
}
