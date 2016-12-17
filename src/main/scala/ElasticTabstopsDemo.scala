import java.awt.{Dimension, Font, FontMetrics}
import javax.swing.UIManager
import javax.swing.text.{AbstractDocument, AttributeSet, DocumentFilter, Element, SimpleAttributeSet, StyleConstants, StyledDocument, TabSet, TabStop}
import javax.swing.text.DocumentFilter.FilterBypass
import swing.{MainFrame, ScrollPane, SimpleSwingApplication, TextPane}

import core.calcMaxedWidthsPerLine
import assets.InitialText

object ElasticTabstopsDemo extends SimpleSwingApplication {

  val CellMinimumWidth = 32
  val CellPaddingWidth = 8

  def alignTabstops(doc: StyledDocument, fm: FontMetrics) {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList
    val textWidthsPerLine = for (element <- elements)
      yield for (text <- doc.getText(element.getStartOffset, element.getEndOffset - element.getStartOffset).split('\t'))
        yield fm.stringWidth(text)

    val maxedWidthsPerLine = calcMaxedWidthsPerLine(textWidthsPerLine, CellMinimumWidth, CellPaddingWidth)

    for ((maxedWidthsThisLine, element) <- maxedWidthsPerLine.zip(elements)) {
      val tabstopPositionsThisLine = (maxedWidthsThisLine.scanLeft(0)(_ + _).drop(1))
      val tabStops = for (tabstopPosition <- tabstopPositionsThisLine) yield new TabStop(tabstopPosition)
      val attributes = new SimpleAttributeSet()
      StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
      doc.setParagraphAttributes(element.getStartOffset, element.getEndOffset, attributes, false)
    }
  }

  def top = new MainFrame {
    title = "Elastic tabstops demo"
    preferredSize = new Dimension(768, 900)
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

    val textPane = new TextPane {
      font = new Font("Merriweather", Font.PLAIN, 15)
      val fontMetrics = peer.getFontMetrics(font)

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offs: Int, str: String, a: AttributeSet) {
          super.insertString(fb, offs, str, a)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def remove(fb: FilterBypass, offs: Int, length: Int) {
          super.remove(fb, offs, length)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def replace(fb: FilterBypass, offs: Int, length: Int, str: String, a: AttributeSet) {
          super.replace(fb, offs, length, str, a)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }
      }

      peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
      peer.setText(InitialText)
    }

    contents = new ScrollPane(textPane)
  }
}
