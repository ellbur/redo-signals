
package redosignals

import java.awt.event.{ItemEvent, ItemListener, ComponentEvent, ComponentListener}
import java.awt.{Dimension, Point, Component}
import javax.swing.text.JTextComponent
import com.github.ellbur.collection.dependentmap.mutable.DependentWeakHashMap
import ellbur.dependenttypes.Depender

import scala.Proxy.Typed
import scala.ref.WeakReference
import javax.swing.event.{ChangeEvent, ChangeListener, DocumentEvent, DocumentListener}
import javax.swing.{JComboBox, JSlider, AbstractButton}
import scala.collection.mutable

trait SwingBridge { self: RedoSignals.type =>
  trait TypedGetSetListen[K <: Depender] {
    def get(x: K): x.T
    def set(x: K)(t: x.T): Unit
    def listen(x: K, f: () => Unit): Unit
  }

  class DependentEventBasedGrabber[K<:Depender](getSetListen: TypedGetSetListen[K]) {
    trait Storage extends Depender {
      val k: K
      type T = (Source[k.T], Observing)
    }

    private val sources = new DependentWeakHashMap[Storage]

    def apply(a: K): BiTarget[a.T] = {
      val weakA: WeakReference[a.type] = WeakReference[a.type](a)
      val init: a.T = getSetListen.get(a)

      object mapKey extends Storage {
        val k: a.type = a
      }

      sources.getOrElseUpdate(mapKey) {
        var inputDisabled: Boolean = false
        var outputDisabled: Boolean = false

        def withInputDisabled[T](b: => T): T = {
          val before = inputDisabled
          try {
            inputDisabled = true
            b
          }
          finally
            inputDisabled = before
        }

        def withOutputDisabled[T](b: => T): T = {
          val before = outputDisabled
          try {
            outputDisabled = true
            b
          }
          finally
            outputDisabled = before
        }

        val src = new Source[a.T](init)
        src.redoObserving.observe(a)
        implicit val obs = new Observing
        weakA.get foreach (a => getSetListen.listen(a, { () =>
          if (!outputDisabled) {
            withInputDisabled {
              weakA.get foreach (a => src() = getSetListen.get(a))
            }
          }
        }))
        src foreach { x =>
          if (!inputDisabled) {
            withOutputDisabled {
              weakA.get foreach (a => getSetListen.set(a)(x))
            }
          }
        }
        (src, obs)
      }._1
    }
  }

  class EventBasedGrabber[A<:AnyRef,P](get: A => P)(set: (A, P) => Unit)(listen: (A, ()  => Unit) => Unit) {
    private val sources = new mutable.WeakHashMap[A,(Source[P],Observing)]
    def apply(a: A): BiTarget[P] = {
      val weakA = WeakReference(a)
      val init = get(a)
      sources.getOrElseUpdate(a, {
        var inputDisabled: Boolean = false
        var outputDisabled: Boolean = false

        def withInputDisabled[T](b: => T): T = {
          val before = inputDisabled
          try {
            inputDisabled = true
            b
          }
          finally
            inputDisabled = before
        }

        def withOutputDisabled[T](b: => T): T = {
          val before = outputDisabled
          try {
            outputDisabled = true
            b
          }
          finally
            outputDisabled = before
        }

        val src = new Source[P](init)
        implicit val obs = new Observing
        weakA.get foreach (a => listen(a, { () =>
          if (!outputDisabled) {
            withInputDisabled {
              weakA.get foreach (a => src() = get(a))
            }
          }
        }))
        src foreach { x =>
          if (!inputDisabled) {
            withOutputDisabled {
              weakA.get foreach (a => set(a, x))
            }
          }
        }
        (src, obs)
      })._1
    }
  }

  private val sliderValue = new EventBasedGrabber[JSlider,Int](_.getValue)(_.setValue(_))((s, l) => s.addChangeListener(new ChangeListener {
    def stateChanged(e: ChangeEvent) { l() }
  }))
  implicit class SliderTargets(slider: JSlider) {
    def value = sliderValue(slider)
  }

  private val textComponentText = new EventBasedGrabber[JTextComponent,String](_.getText)(_.setText(_))((t,l) => t.getDocument.addDocumentListener(new DocumentListener {
    def insertUpdate(e: DocumentEvent) { l() }
    def changedUpdate(e: DocumentEvent) { l() }
    def removeUpdate(e: DocumentEvent) { l() }
  }))
  implicit class TextComponentTargets(textComponent: JTextComponent) {
    def text = textComponentText(textComponent)
  }

  private val abstractButtonSelected = new EventBasedGrabber[AbstractButton,Boolean](_.isSelected)(_.setSelected(_))((b,l) => b.addChangeListener(new ChangeListener {
    def stateChanged(e: ChangeEvent) { l() }
  }))
  implicit class AbstractButtonTargets(button: AbstractButton) {
    def selected = abstractButtonSelected(button)
  }

  private val componentLocation = new EventBasedGrabber[Component,Point](_.getLocation)(_.setLocation(_))((c,l) => c.addComponentListener(new ComponentListener {
    def componentShown(e: ComponentEvent) { }
    def componentHidden(e: ComponentEvent) { }
    def componentMoved(e: ComponentEvent) { l() }
    def componentResized(e: ComponentEvent) { }
  }))
  private val componentSize = new EventBasedGrabber[Component,Dimension](_.getSize)(_.setSize(_))((c,l) => c.addComponentListener(
    new ComponentListener {
      def componentShown(e: ComponentEvent) { }
      def componentHidden(e: ComponentEvent) { }
      def componentMoved(e: ComponentEvent) { }
      def componentResized(e: ComponentEvent) { l() }
    }
  ))
  implicit class ComponentTargets(c: Component) {
    def currentLocation = componentLocation(c)
    def currentSize = componentSize(c)
  }

  trait TypedComboBox extends Depender {
    type I
    type T = Option[I]
    val comboBox: JComboBox[I]
  }

  private val comboBoxItem = new DependentEventBasedGrabber[TypedComboBox](new TypedGetSetListen[TypedComboBox] {
    override def get(x: TypedComboBox): x.T = Option(x.comboBox.getSelectedItem.asInstanceOf[x.I])
    override def listen(x: TypedComboBox, f: () => Unit): Unit = x.comboBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent) {
        f()
      }
    })
    override def set(x: TypedComboBox)(t: x.T): Unit = {
      x.comboBox.setSelectedItem(t.orNull)
    }
  })
  def comboBoxSelectedItem[Z](_comboBox: JComboBox[Z]) = comboBoxItem(new TypedComboBox {
    type I = Z
    override val comboBox: JComboBox[I] = _comboBox
  })
  implicit class ComboBoxTargets[T](comboBox: JComboBox[T]) {
    def selectedItem = comboBoxSelectedItem(comboBox)
  }

  def alwaysChanging[T](target: Target[T]): reactive.EventStream[T] = {
    val changed = new reactive.EventSource[T] {
      val observing = new Observing
    }
    target.foreach(changed.fire)(changed.observing)

    changed
  }

  def alwaysChangingWhenValid[T](target: Target[Option[T]]): reactive.EventStream[T] = {
    val changed = new reactive.EventSource[T] {
      val observing = new Observing
    }
    target.foreach(_ foreach changed.fire)(changed.observing)

    changed
  }
}
