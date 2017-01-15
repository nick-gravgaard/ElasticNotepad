import java.awt.{Dimension, Font, FontMetrics}
import javax.swing.UIManager
import javax.swing.text.{AbstractDocument, AttributeSet, DocumentFilter, SimpleAttributeSet, StyleConstants, StyledDocument, TabSet, TabStop}
import javax.swing.text.DocumentFilter.FilterBypass
import scala.swing.{Action, Dialog, MainFrame, Menu, MenuBar, MenuItem, ScrollPane, Separator, SimpleSwingApplication, TextPane}
import scala.util.Try

import assets.InitialText
import core.{calcTabstopPositions, getCellsPerLine, toSpaces}
import filehandling.{loadFile, saveFile, saveFileAs}

object ElasticTabstopsDemo extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  val CellMinimumWidth = 32
  val CellPaddingWidth = 8
  val NofIndentSpaces = 4

  def alignTabstops(doc: StyledDocument, fm: FontMetrics): Unit = {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList

    val textPerLine = for (el <- elements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)
    val cellsPerLine = getCellsPerLine(textPerLine)
    def calcCellWidth(text: String): Int = math.max(fm.stringWidth(text), CellMinimumWidth) + CellPaddingWidth
    for ((tabstopPositionsThisLine, element) <- calcTabstopPositions(cellsPerLine, calcCellWidth).zip(elements)) {
      val tabStops = for (tabstopPosition <- tabstopPositionsThisLine) yield new TabStop(tabstopPosition)
      val attributes = new SimpleAttributeSet()
      StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
      doc.setParagraphAttributes(element.getStartOffset, element.getEndOffset, attributes, false)
    }
  }

  var currentPath: Option[String] = None

  def makeWindowTitleText(pathOption: Option[String], modified: Boolean): String = {
    s"${if (modified) "* " else ""}${pathOption.getOrElse("Not saved yet")} - Elastic tabstops demo"
  }

  def top = new MainFrame {
    title = makeWindowTitleText(currentPath, false)

    def newFileAction(): Unit = {
      textPane.text = ""
      currentPath = None
      setWindowTitle(makeWindowTitleText(currentPath, false))
    }

    def loadFileAction(): Unit = {
      loadFile foreach { case (loadedText, path) =>
        textPane.text = loadedText
        currentPath = Some(path)
        setWindowTitle(makeWindowTitleText(currentPath, false))
      }
    }

    def saveFileAction(): Unit = {
      currentPath match {
        case Some(path) => {
          saveFile(toSpaces(textPane.text, NofIndentSpaces), path)
          setWindowTitle(makeWindowTitleText(currentPath, false))
        }
        case None => saveFileAs(textPane.text) foreach { path =>
          currentPath = Some(path)
          setWindowTitle(makeWindowTitleText(currentPath, false))
        }
      }
    }

    def saveFileAsAction(): Unit = {
      saveFileAs(toSpaces(textPane.text, NofIndentSpaces)) foreach { path =>
        currentPath = Some(path)
        setWindowTitle(makeWindowTitleText(currentPath, false))
      }
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("New") { newFileAction })
        contents += new MenuItem(Action("Open...") { loadFileAction })
        contents += new Separator
        contents += new MenuItem(Action("Save") { saveFileAction })
        contents += new MenuItem(Action("Save as...") { saveFileAsAction })
      }
    }

    preferredSize = new Dimension(768, 900)

    val textPane = new TextPane {
      font = new Font("Merriweather", Font.PLAIN, 15)
      val fontMetrics = peer.getFontMetrics(font)

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offs: Int, str: String, a: AttributeSet) {
          super.insertString(fb, offs, str, a)
          setWindowTitle(makeWindowTitleText(currentPath, true))
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def remove(fb: FilterBypass, offs: Int, length: Int) {
          super.remove(fb, offs, length)
          setWindowTitle(makeWindowTitleText(currentPath, true))
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def replace(fb: FilterBypass, offs: Int, length: Int, str: String, a: AttributeSet) {
          super.replace(fb, offs, length, str, a)
          setWindowTitle(makeWindowTitleText(currentPath, true))
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }
      }

      peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
      peer.setText(InitialText)
    }

    def setWindowTitle(newTitle: String): Unit = peer.setTitle(newTitle)

    contents = new ScrollPane(textPane)
  }

}
