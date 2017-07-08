import java.awt.{Canvas, Dimension, Font, FontMetrics}
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.text._
import javax.swing.{UIManager, WindowConstants}

import assets.InitialText
import core.{calcTabstopPositions, spacesToTabs, tabsToSpaces}
import filehandling.{loadFile, saveFile, saveFileAs}

import scala.swing.BorderPanel.Position.{Center, North}
import scala.swing.Dialog.Result
import scala.swing.FlowPanel.Alignment.Left
import scala.swing.event.ButtonClicked
import scala.swing.{Action, BorderPanel, Dialog, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, ScrollPane, Separator, SimpleSwingApplication, TextPane, ToggleButton}

object ElasticTabstopsDemo extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  val proportionalFont = ("Merriweather", 18)
  val monospacedFont = ("Droid Sans Mono", 18)

  val fontMetrics = new Canvas().getFontMetrics(new Font(proportionalFont._1, Font.PLAIN, proportionalFont._2))
  val em = fontMetrics.getHeight // more accurate than the font's point size (with Merriweather at least)
  val EmptyColumnWidth = (2.0 * em).toInt
  val MinGapBetweenText = (0.6666 * em).toInt
  val EmptyColumnWidthMinusGap = EmptyColumnWidth - MinGapBetweenText
  val NofIndentSpaces = 4

  def alignTabstops(doc: StyledDocument, fm: FontMetrics): Unit = {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList

    val textPerLine = for (el <- elements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)
    val cellsPerLine = textPerLine.map(_.split('\t').toList)
    def calcCellWidth(text: String): Int = math.max(fm.stringWidth(text), EmptyColumnWidthMinusGap) + MinGapBetweenText
    for ((tabstopPositionsThisLine, element) <- calcTabstopPositions(cellsPerLine, calcCellWidth).zip(elements)) {
      val tabStops = tabstopPositionsThisLine.map(new TabStop(_))
      val attributes = new SimpleAttributeSet()
      StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
      doc.setParagraphAttributes(element.getStartOffset, element.getEndOffset, attributes, false)
    }
  }

  var currentPath: Option[String] = None

  var modified = false

  def makeWindowTitleText(pathOption: Option[String]): String = {
    s"${if (modified) "* " else ""}${pathOption.getOrElse("Not saved yet")} - Elastic tabstops demo"
  }

  def top = new MainFrame {
    title = makeWindowTitleText(currentPath)
    preferredSize = new Dimension(1072, 1072)

    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    override def closeOperation() = {
      if (!modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to close this window?") == Result.Ok) {
        dispose()
      }
    }

    def newFileAction(): Unit = {
      textPane.text = ""
      currentPath = None
      modified = false
      setWindowTitle(makeWindowTitleText(currentPath))
    }

    def loadFileAction(): Unit = {
      loadFile foreach { case (loadedText, path) =>
        textPane.text = spacesToTabs(loadedText)
        currentPath = Some(path)
        modified = false
        setWindowTitle(makeWindowTitleText(currentPath))
      }
    }

    def saveFileAction(): Unit = {
      currentPath match {
        case Some(path) => {
          saveFile(tabsToSpaces(textPane.text, NofIndentSpaces), path)
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        }
        case None => saveFileAs(textPane.text) foreach { path =>
          currentPath = Some(path)
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        }
      }
    }

    def saveFileAsAction(): Unit = {
      saveFileAs(tabsToSpaces(textPane.text, NofIndentSpaces)) foreach { path =>
        currentPath = Some(path)
        modified = false
        setWindowTitle(makeWindowTitleText(currentPath))
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

    def setElasticTabstopsDocFilter(textPane: TextPane) = {
      val fontMetrics = textPane.peer.getFontMetrics(textPane.font)

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offs: Int, str: String, a: AttributeSet) {
          super.insertString(fb, offs, str, a)
          modified = true
          setWindowTitle(makeWindowTitleText(currentPath))
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def remove(fb: FilterBypass, offs: Int, length: Int) {
          super.remove(fb, offs, length)
          modified = true
          setWindowTitle(makeWindowTitleText(currentPath))
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def replace(fb: FilterBypass, offs: Int, length: Int, str: String, a: AttributeSet) {
          super.replace(fb, offs, length, str, a)
          modified = true
          setWindowTitle(makeWindowTitleText(currentPath))
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
