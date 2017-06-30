import java.awt.{Dimension, Font, FontMetrics}
import javax.swing.UIManager
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.text._

import assets.InitialText
import core.{calcTabstopPositions, spacesToTabs, tabsToSpaces}
import filehandling.{loadFile, saveFile, saveFileAs}

import scala.swing.BorderPanel.Position.{Center, North}
import scala.swing.FlowPanel.Alignment.Left
import scala.swing.event.ButtonClicked
import scala.swing.{Action, BorderPanel, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, ScrollPane, Separator, SimpleSwingApplication, TextPane, ToggleButton}

object ElasticTabstopsDemo extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  val CellMinimumWidth = 32
  val CellPaddingWidth = 8
  val NofIndentSpaces = 4

  val proportionalFont = ("Merriweather", 15)
  val monospacedFont = ("Droid Sans Mono", 15)

  def alignTabstops(doc: StyledDocument, fm: FontMetrics): Unit = {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList

    val textPerLine = for (el <- elements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)
    val cellsPerLine = textPerLine.map(_.split('\t').toList)
    def calcCellWidth(text: String): Int = math.max(fm.stringWidth(text), CellMinimumWidth) + CellPaddingWidth
    for ((tabstopPositionsThisLine, element) <- calcTabstopPositions(cellsPerLine, calcCellWidth).zip(elements)) {
      val tabStops = tabstopPositionsThisLine.map(new TabStop(_))
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
        textPane.text = spacesToTabs(loadedText)
        currentPath = Some(path)
        setWindowTitle(makeWindowTitleText(currentPath, false))
      }
    }

    def saveFileAction(): Unit = {
      currentPath match {
        case Some(path) => {
          saveFile(tabsToSpaces(textPane.text, NofIndentSpaces), path)
          setWindowTitle(makeWindowTitleText(currentPath, false))
        }
        case None => saveFileAs(textPane.text) foreach { path =>
          currentPath = Some(path)
          setWindowTitle(makeWindowTitleText(currentPath, false))
        }
      }
    }

    def saveFileAsAction(): Unit = {
      saveFileAs(tabsToSpaces(textPane.text, NofIndentSpaces)) foreach { path =>
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

    preferredSize = new Dimension(896, 960)

    def setElasticTabstopsDocFilter(textPane: TextPane) = {
      val fontMetrics = textPane.peer.getFontMetrics(textPane.font)

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
      textPane.peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
    }

    val textPane = new TextPane { font = new Font(proportionalFont._1, Font.PLAIN, proportionalFont._2) }
    setElasticTabstopsDocFilter(textPane)
    textPane.peer.setText(InitialText)

    def setWindowTitle(newTitle: String): Unit = peer.setTitle(newTitle)

    val elasticToggle = new ToggleButton { text = "Elastic on"; selected = true }
    val flowPanel = new FlowPanel(Left)(elasticToggle)
    val scrollPane = new ScrollPane(textPane)

    contents = new BorderPanel {
      layout(flowPanel) = North
      layout(scrollPane) = Center
    }

    listenTo(elasticToggle)

    def setFont(fontDetails: (String, Int)) = {
      val attributes = new SimpleAttributeSet
      StyleConstants.setFontFamily(attributes, fontDetails._1)
      StyleConstants.setFontSize(attributes, fontDetails._2)
      val doc = textPane.peer.getDocument.asInstanceOf[StyledDocument]
      doc.setParagraphAttributes(0, doc.getLength, attributes, true)
    }

    def turnElasticTabstopsOn = {
      elasticToggle.text = "Elastic on"
      setFont(proportionalFont)
      setElasticTabstopsDocFilter(textPane)
      textPane.peer.setText(spacesToTabs(textPane.text))
    }

    def turnElasticTabstopsOff = {
      elasticToggle.text = "Elastic off"
      setFont(monospacedFont)
      textPane.peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(new DocumentFilter)
      textPane.peer.setText(tabsToSpaces(textPane.text, 4))
    }

    reactions += {
      case ButtonClicked(component) if component == elasticToggle =>
        elasticToggle.selected match {
          case true => turnElasticTabstopsOn
          case false => turnElasticTabstopsOff
        }
    }
  }

}
