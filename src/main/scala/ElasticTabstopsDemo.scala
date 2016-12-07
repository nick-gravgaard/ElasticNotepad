import java.awt.Dimension

import swing._
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.text.{AbstractDocument, AttributeSet, DocumentFilter}

import javax.swing.UIManager
import java.awt.{Font, FontMetrics}
import javax.swing.text._

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

  val TabMinimumWidth = 32
  val TabPaddingWidth = 8

  class Cell(var contents: String = "", var width: Int = 0)

  def stretchTabstops(doc: StyledDocument, fm: FontMetrics) {
    val section = doc.getDefaultRootElement

    def getLinesCells(line: Element): Array[Cell] = {
      val lineText = doc.getText(line.getStartOffset, line.getEndOffset - line.getStartOffset)
      for (cellText <- lineText.split('\t')) yield new Cell(contents = cellText)
    }
    val cellsPerLine = for (l <- 0 until section.getElementCount) yield getLinesCells(section.getElement(l))

    for ((cellsThisLine, l) <- cellsPerLine.view.zipWithIndex) {
      for (c <- cellsThisLine.indices.dropRight(1)) {
        if (l == 0 || c >= cellsPerLine(l - 1).length - 1) {  // is there no matching cell on the previous line?
          // first cell in column block - get max cell width and set for cells in column block
          val columnBlock = for (line <- cellsPerLine.drop(l).takeWhile(c < _.length - 1)) yield line(c)
          val maxWidth = (for (cell <- columnBlock) yield calcTabWidth(fm.stringWidth(cell.contents))).max
          for (cell <- columnBlock)
            cell.width = maxWidth
        }
      }
    }

    for ((cellsThisLine, l) <- cellsPerLine.view.zipWithIndex) {
      val line = section.getElement(l)
      var accTabstop = 0
      for (cell <- cellsThisLine) {
        accTabstop += cell.width
        cell.width = accTabstop
      }
      setBlocksTabstops(doc, line.getStartOffset, line.getEndOffset, cellsThisLine)
    }
  }

  def calcTabWidth(textWidthInTab: Int): Int = {
    math.max(textWidthInTab, TabMinimumWidth) + TabPaddingWidth
  }

  def setBlocksTabstops(doc: StyledDocument, start: Int, length: Int, tabstopPositions: Array[Cell]) {
    val tabs = for (tabstopPosition <- tabstopPositions) yield new TabStop(tabstopPosition.width)
    val tabSet = new TabSet(tabs.toArray)
    val attributes = new SimpleAttributeSet()
    StyleConstants.setTabSet(attributes, tabSet)
    doc.setParagraphAttributes(start, length, attributes, false)
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
          stretchTabstops(doc, fontMetrics)
        }

        override def remove(fb: FilterBypass, offs: Int, length: Int) {
          super.remove(fb, offs, length)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          stretchTabstops(doc, fontMetrics)
        }

        override def replace(fb: FilterBypass, offs: Int, length: Int, str: String, a: AttributeSet) {
          super.replace(fb, offs, length, str, a)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          stretchTabstops(doc, fontMetrics)
        }
      }

      peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
      peer.setText(initialText)
    }

    contents = new ScrollPane(textPane)
  }
}