import java.awt.{Dimension, FileDialog, Font, FontMetrics}
import java.io.{BufferedWriter, FileNotFoundException, FileOutputStream, IOException, OutputStreamWriter}
import javax.swing.UIManager
import javax.swing.text.{AbstractDocument, AttributeSet, DocumentFilter, SimpleAttributeSet, StyleConstants, StyledDocument, TabSet, TabStop}
import javax.swing.text.DocumentFilter.FilterBypass
import scala.io.Source
import scala.swing.{Action, Dialog, MainFrame, Menu, MenuBar, MenuItem, ScrollPane, Separator, SimpleSwingApplication, TextPane}
import scala.util.control.NonFatal

import core.calcTabstopPositions
import assets.InitialText

object ElasticTabstopsDemo extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  val CellMinimumWidth = 32
  val CellPaddingWidth = 8

  def alignTabstops(doc: StyledDocument, fm: FontMetrics) {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList

    val textPerLine = for (el <- elements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)
    def calcCellWidth(text: String): Int = math.max(fm.stringWidth(text), CellMinimumWidth) + CellPaddingWidth
    for ((tabstopPositionsThisLine, element) <- calcTabstopPositions(textPerLine, calcCellWidth).zip(elements)) {
      val tabStops = for (tabstopPosition <- tabstopPositionsThisLine) yield new TabStop(tabstopPosition)
      val attributes = new SimpleAttributeSet()
      StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
      doc.setParagraphAttributes(element.getStartOffset, element.getEndOffset, attributes, false)
    }
  }

  var currentPath: Option[String] = None

  def top = new MainFrame {
    setWindowTitle(currentPath, false)

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("New") {
          newFile
        })
        contents += new MenuItem(Action("Open...") {
          loadFile
        })
        contents += new Separator
        contents += new MenuItem(Action("Save") {
          currentPath match {
            case Some(path) => saveFile(path)
            case None => saveFileAs
          }
        })
        contents += new MenuItem(Action("Save as...") {
          saveFileAs
        })
      }
    }

    preferredSize = new Dimension(768, 900)

    val textPane = new TextPane {
      font = new Font("Merriweather", Font.PLAIN, 15)
      val fontMetrics = peer.getFontMetrics(font)

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offs: Int, str: String, a: AttributeSet) {
          super.insertString(fb, offs, str, a)
          setWindowTitle(currentPath, true)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def remove(fb: FilterBypass, offs: Int, length: Int) {
          super.remove(fb, offs, length)
          setWindowTitle(currentPath, true)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def replace(fb: FilterBypass, offs: Int, length: Int, str: String, a: AttributeSet) {
          super.replace(fb, offs, length, str, a)
          setWindowTitle(currentPath, true)
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }
      }

      peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
      peer.setText(InitialText)
    }

    def setWindowTitle(pathOption: Option[String], modified: Boolean) {
      peer.setTitle(s"${if (modified) "* " else ""}${pathOption.getOrElse("Not saved yet")} - Elastic tabstops demo")
    }

    def newFile() {
      textPane.text = ""
      currentPath = None
      setWindowTitle(currentPath, false)
    }

    def loadFile() {
      val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Load file", FileDialog.LOAD)
      dialog.setVisible(true)
      if (dialog.getFile != null) {
        val path = dialog.getDirectory + dialog.getFile
        try {
          val fileSource = Source.fromFile(path, "UTF-8")
          try {
            textPane.text = fileSource.getLines.mkString("\n")
          } catch {
            case e: IOException => Dialog.showMessage(null, s"Can't load $path\n[${e.getMessage}]")
            case NonFatal(e) => Dialog.showMessage(null, s"Unknown non-fatal exception trying to load $path\n[${e.getMessage}]")
          } finally {
            fileSource.close
            currentPath = Some(path)
            setWindowTitle(currentPath, false)
          }
        } catch {
          case e: FileNotFoundException => Dialog.showMessage(null, s"Can't find $path\n[${e.getMessage}]")
          case NonFatal(e) => Dialog.showMessage(null, s"Unknown non-fatal exception trying to find $path\n[${e.getMessage}]")
        }
      }
    }

    def saveFile(path: String) {
      println(path)
      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), "UTF-8"))
      try {
        writer.write(textPane.text)
        writer.flush
        currentPath = Some(path)
        setWindowTitle(currentPath, false)
      } catch {
        case e: IOException => Dialog.showMessage(null, s"Can't save $path\n[${e.getMessage}]")
        case NonFatal(e) => Dialog.showMessage(null, s"Unknown non-fatal exception trying to save $path\n[${e.getMessage}]")
      } finally {
        writer.close
      }
    }

    def saveFileAs() {
      val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Save file", FileDialog.SAVE)
      dialog.setVisible(true)
      if (dialog.getFile != null)
        saveFile(dialog.getDirectory + dialog.getFile)
    }

    contents = new ScrollPane(textPane)
  }

}
