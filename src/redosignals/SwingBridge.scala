
package redosignals

import javax.swing.text.JTextComponent
import scala.ref.WeakReference
import javax.swing.event.{ChangeEvent, ChangeListener, DocumentEvent, DocumentListener}
import javax.swing.AbstractButton

trait SwingBridge { self: RedoSignals.type =>
  def weakTextComponentText(init: String, textComponent: WeakReference[JTextComponent]) = {
    val src = new Source[String](init)

    textComponent.get foreach (_.getDocument.addDocumentListener(new DocumentListener {
      def changed() {
        textComponent.get foreach (t => src() = t.getText)
      }

      def insertUpdate(e: DocumentEvent) { changed() }
      def changedUpdate(e: DocumentEvent) { changed() }
      def removeUpdate(e: DocumentEvent) { changed() }
    }))

    src
  }

  implicit class TextComponentTargets(textComponent: JTextComponent) {
    def text = weakTextComponentText(textComponent.getText, WeakReference(textComponent))
  }

  def weakAbstractButtonSelected(init: Boolean, button: WeakReference[AbstractButton]) = {
    val src = new Source[Boolean](init)

    button.get foreach (_.addChangeListener(new ChangeListener {
      def stateChanged(e: ChangeEvent) {
        button.get foreach (b => src() = b.isSelected)
      }
    }))

    src
  }

  implicit class AbstractButtonTargets(button: AbstractButton) {
    def selected = weakAbstractButtonSelected(button.isSelected, WeakReference(button))
  }
}
