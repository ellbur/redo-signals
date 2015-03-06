
import collection.mutable

object DPTest extends App {
  case class Level(date: Int, depth: Int)

  implicit object LevelOrdering extends Ordering[Level] {
    override def compare(x: Level, y: Level): Int =
      y.date.compareTo(x.date) match {
        case 0 => x.depth.compareTo(y.depth)
        case other => other
      }
  }

  implicit object OptionLevelOrdering extends Ordering[Option[Level]] {
    override def compare(x: Option[Level], y: Option[Level]): Int =
      (x, y) match {
        case (None, None) => 0
        case (None, _) => +1
        case (_, None) => -1
        case (Some(x), Some(y)) => implicitly[Ordering[Level]].compare(x, y)
      }
  }

  sealed trait ChildRule
  case class SimpleChildren(children: Seq[Node]) extends ChildRule
  case class ValueStepChildren(primaryChildren: Seq[Node], secondaryChildren: () => Seq[Node]) extends ChildRule
  case class LevelStepChildren(primaryChildren: Seq[Node], secondaryChildren: () => Seq[Node]) extends ChildRule

  trait Node {
    var level: Option[Level] = None
    def calculate()
    val childRule: ChildRule
  }

  def process(root: Node): Unit = {

  }
}
