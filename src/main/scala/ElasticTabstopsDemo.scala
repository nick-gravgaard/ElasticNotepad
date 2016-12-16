import java.awt.{Dimension, Font, FontMetrics}
import javax.swing.UIManager
import javax.swing.text.{AbstractDocument, AttributeSet, DocumentFilter, Element, SimpleAttributeSet, StyleConstants, StyledDocument, TabSet, TabStop}
import javax.swing.text.DocumentFilter.FilterBypass
import swing.{MainFrame, ScrollPane, SimpleSwingApplication, TextPane}

object ElasticTabstopsDemo extends SimpleSwingApplication {
  val initialText = StringContext.treatEscapes(
    """/* Hopefully this editor widget should demonstrate how elastic tabstops work.\t*/
      |/* Try inserting and deleting different parts of the text and watch as the tabstops move.\t*/
      |/* If you like this, please ask the writers of your text editor to implement it.\t*/
      |
      |#include <stdio.h>
      |
      |struct ipc_perm
      |{
      |\tkey_t\tkey;
      |\tushort\tuid;\t/* owner euid and egid\t*/
      |\tushort\tgid;\t/* group id\t*/
      |\tushort\tcuid;\t/* creator euid and egid\t*/
      |\tcell-missing\t\t/* for test purposes\t*/
      |\tushort\tmode;\t/* access modes\t*/
      |\tushort\tseq;\t/* sequence number\t*/
      |};
      |
      |int someDemoCode(\tint fred,
      |\tint wilma)
      |{
      |\tx();\t/* try making\t*/
      |\tprintf(\"hello!\\");\t/* this comment\t*/
      |\tdoSomethingComplicated();\t/* a bit longer\t*/
      |\tfor (i = start; i < end; ++i)
      |\t{
      |\t\tif (isPrime(i))
      |\t\t{
      |\t\t\t++numPrimes;
      |\t\t}
      |\t}
      |\treturn numPrimes;
      |}
      |
      |---- and now for something completely different: a table ----
      |
      |Title\tAuthor\tPublisher\tYear
      |Generation X\tDouglas Coupland\tAbacus\t1995
      |Informagic\tJean-Pierre Petit\tJohn Murray Ltd\t1982
      |The Cyberiad\tStanislaw Lem\tHarcourt Publishers Ltd\t1985
      |The Selfish Gene\tRichard Dawkins\tOxford University Press\t2006""".stripMargin
  )

  val CellMinimumWidth = 32
  val CellPaddingWidth = 8

  def calcCellWidth(textWidthInCell: Int): Int = math.max(textWidthInCell, CellMinimumWidth) + CellPaddingWidth

  def maxConsecutive(list: List[Option[Int]]) : List[Option[Int]] = list match {
    // scala>     maxConsecutive(List(Some(1), Some(2), None, Some(4), None, None, Some(7), Some(8), Some(9)))
    // res1: List[Option[Int]] = List(Some(2), Some(2), None, Some(4), None, None, Some(9), Some(9), Some(9))
    case Nil => Nil
    case h::t => h match {
      case None => None :: maxConsecutive(list.drop(1))
      case Some(cell) => {
        val segment = list.takeWhile(_.isDefined).map(_.get)
        List.fill(segment.length)(Option(segment.max)) ::: maxConsecutive(list.drop(segment.length))
      }
    }
  }

  def calcMaxedWidthsPerLine(textWidthsPerLine: List[Array[Int]]) : List[List[Int]] = {
    val maxNofCells = (for (textWidthsThisLine <- textWidthsPerLine) yield textWidthsThisLine.length).max

    val maxedWidthsPerColumn = for (c <- 0 until maxNofCells)
      yield maxConsecutive(for (textWidthsThisLine <- textWidthsPerLine)
        yield if (c < textWidthsThisLine.indices.last) Option(calcCellWidth(textWidthsThisLine(c))) else None)

    for (maxedWidthsThisLine <- maxedWidthsPerColumn.toList.transpose)
      yield maxedWidthsThisLine.takeWhile(_.isDefined).map(_.get)
  }

  def alignTabstops(doc: StyledDocument, fm: FontMetrics) {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList
    val textWidthsPerLine = for (element <- elements)
      yield for (text <- doc.getText(element.getStartOffset, element.getEndOffset - element.getStartOffset).split('\t'))
        yield fm.stringWidth(text)

    val maxedWidthsPerLine = calcMaxedWidthsPerLine(textWidthsPerLine)

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
      peer.setText(initialText)
    }

    contents = new ScrollPane(textPane)
  }
}
