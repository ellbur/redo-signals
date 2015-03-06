
import javax.swing.JTextField
import redosignals.Observing
import redosignals.RedoSignals._

object BlahTest extends App {
  {
    implicit val obs = new Observing
    val tf = new JTextField()

    val v = tracking { implicit t =>
      println("Rerunning tracker")
      println(s"Swing text is now ${tf.getText}")
      tf.text.track
    }

    v foreach (x => println(s"Got $x"))

    tf.setText("a")
    tf.setText("ab")
    tf.setText("abc")
    tf.setText("abcd")
  }
}
