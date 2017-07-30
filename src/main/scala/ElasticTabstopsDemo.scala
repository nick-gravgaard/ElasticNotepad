import java.awt.{Canvas, Dimension, Font, FontMetrics}
import java.awt.Event.{CTRL_MASK, SHIFT_MASK}
import java.awt.event.KeyEvent.{VK_N, VK_O, VK_S}
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.text._
import javax.swing.{KeyStroke, UIManager, WindowConstants}

import core.{calcTabstopPositions, spacesToTabs, tabsToSpaces}
import filehandling.{chooseAndLoadFile, loadScratchFile, saveFile, saveFileAs, scratchFilePath}
import settings.{FontCC, Settings}

import scala.swing.BorderPanel.Position.{Center, North}
import scala.swing.Dialog.Result
import scala.swing.FlowPanel.Alignment.Left
import scala.swing.event.ButtonClicked
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Dialog, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, Orientation, ScrollPane, Separator, SimpleSwingApplication, TextPane, ToggleButton}

object ElasticTabstopsDemo extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  var (currentSettings, currentSettingsText) = Settings.load

  def scaleUiFonts(multiplier: Float) = {
    UIManager.getLookAndFeelDefaults.keySet.forEach { key =>
      val keyName = key.toString
      if (keyName.endsWith(".font")) {
        val font = UIManager.getFont(keyName)
        val biggerFont = font.deriveFont(multiplier * font.getSize2D)
        UIManager.put(keyName, biggerFont)
      }
    }
  }

  scaleUiFonts(1.75f)

  def alignTabstops(doc: StyledDocument, fm: FontMetrics): Unit = {
    val section = doc.getDefaultRootElement
    val elements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList

    val textPerLine = for (el <- elements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)
    val cellsPerLine = textPerLine.map(_.split('\t').toList)
    def calcCellWidth(text: String): Int = math.max(fm.stringWidth(text), currentSettings.emptyColumnWidthMinusGapPx) + currentSettings.minGapBetweenTextPx
    for ((tabstopPositionsThisLine, element) <- calcTabstopPositions(cellsPerLine, calcCellWidth).zip(elements)) {
      val tabStops = tabstopPositionsThisLine.map(new TabStop(_))
      val attributes = new SimpleAttributeSet()
      StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
      doc.setParagraphAttributes(element.getStartOffset, element.getEndOffset, attributes, false)
    }
  }

  var currentPath = scratchFilePath.toString

  var modified = false

  def makeWindowTitleText(path: String): String = {
    s"${if (modified) "* " else ""}$path - Elastic tabstops demo"
  }

  def top = new MainFrame {
    title = makeWindowTitleText(currentPath)
    preferredSize = new Dimension(1536, 1024)
    maximize

    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    override def closeOperation() = {
      if (!modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to close this window?") == Result.Ok) {
        dispose()
      }
    }

    def scratchFileAction(): Action = {
      val action = Action("Open scratch file") {
        if (currentPath != scratchFilePath.toString && (!modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to switch to the scratch file?") == Result.Ok)) {
          textPane.text = loadScratchFile
          currentPath = scratchFilePath.toString
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_N, CTRL_MASK))
      action
    }

    def loadFileAction(): Action = {
      val action = Action("Open...") {
        chooseAndLoadFile(currentSettings.filesEndWithNewline) foreach { case (loadedText, path) =>
          textPane.text = if (currentSettings.filesUseSpaces) spacesToTabs(loadedText) else loadedText
          currentPath = path
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_O, CTRL_MASK))
      action
    }

    def saveFileAction(): Action = {
      val action = Action("Save") {
        val textToSave = if (currentSettings.filesUseSpaces) tabsToSpaces(textPane.text, currentSettings.nofIndentSpaces) else textPane.text
        saveFile(textToSave, currentSettings.filesEndWithNewline, currentPath)
        modified = false
        setWindowTitle(makeWindowTitleText(currentPath))
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_S, CTRL_MASK))
      action
    }

    def saveFileAsAction(): Action = {
      val action = Action("Save as...") {
        val textToSave = if (currentSettings.filesUseSpaces) tabsToSpaces(textPane.text, currentSettings.nofIndentSpaces) else textPane.text
        saveFileAs(textToSave, currentSettings.filesEndWithNewline) foreach { path =>
          currentPath = path
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_S, CTRL_MASK | SHIFT_MASK))
      action
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(scratchFileAction)
        contents += new MenuItem(loadFileAction)
        contents += new Separator
        contents += new MenuItem(saveFileAction)
        contents += new MenuItem(saveFileAsAction)
      }
    }

    def setWindowTitle(newTitle: String): Unit = peer.setTitle(newTitle)

    def onTextPaneChangeSetModified() = {
      modified = true
      setWindowTitle(makeWindowTitleText(currentPath))
    }

    def setElasticTabstopsDocFilter(textPane: TextPane, f: FontCC, onChange: () => Unit = { () => Unit }) = {
      val fontMetrics = new Canvas().getFontMetrics(new Font(f.name, Font.PLAIN, f.size))

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offs: Int, str: String, a: AttributeSet) {
          super.insertString(fb, offs, str, a)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def remove(fb: FilterBypass, offs: Int, length: Int) {
          super.remove(fb, offs, length)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }

        override def replace(fb: FilterBypass, offs: Int, length: Int, str: String, a: AttributeSet) {
          super.replace(fb, offs, length, str, a)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          alignTabstops(doc, fontMetrics)
        }
      }
      textPane.peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
    }

    val textPane = new TextPane { font = new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size) }
    setElasticTabstopsDocFilter(textPane, currentSettings.elasticFont, onTextPaneChangeSetModified)
    textPane.text = loadScratchFile
    modified = false
    setWindowTitle(makeWindowTitleText(currentPath))

    val elasticToggle = new ToggleButton { text = "Elastic on"; selected = true }
    val settingsToggle = new ToggleButton { text = "Settings"; selected = false }
    val toolbarPanel = new FlowPanel(Left)(elasticToggle, settingsToggle)
    val settingsTextPane = new TextPane { font = new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size) }
    setElasticTabstopsDocFilter(settingsTextPane, currentSettings.elasticFont)
    settingsTextPane.text = currentSettingsText
    settingsTextPane.background = this.background

    val saveAndApplySettingsButton = new Button("Save and apply")
    val revertToDefaultSettingsButton = new Button("Revert to defaults")
    val settingsToolbarPanel = new FlowPanel(Left)(saveAndApplySettingsButton, revertToDefaultSettingsButton)

    val settingsPanel = new BoxPanel(Orientation.Vertical) {
      visible = false
      contents += new Separator
      contents += settingsTextPane
      contents += settingsToolbarPanel
    }

    val toolbarAndSettingsPanel = new BoxPanel(Orientation.Vertical) {
      contents += toolbarPanel
      contents += settingsPanel
    }
    val scrollPane = new ScrollPane(textPane)

    contents = new BorderPanel {
      layout(toolbarAndSettingsPanel) = North
      layout(scrollPane) = Center
    }

    listenTo(elasticToggle)
    listenTo(settingsToggle)
    listenTo(saveAndApplySettingsButton)
    listenTo(revertToDefaultSettingsButton)

    def setFont(textPane: TextPane, fontDetails: FontCC) = {
      val attributes = new SimpleAttributeSet
      StyleConstants.setFontFamily(attributes, fontDetails.name)
      StyleConstants.setFontSize(attributes, fontDetails.size)
      val doc = textPane.peer.getDocument.asInstanceOf[StyledDocument]
      doc.setParagraphAttributes(0, doc.getLength, attributes, true)
    }

    def turnElasticTabstopsOn = {
      elasticToggle.text = "Elastic on"
      setFont(textPane, currentSettings.elasticFont)
      setElasticTabstopsDocFilter(textPane, currentSettings.elasticFont, onTextPaneChangeSetModified)
      textPane.text = spacesToTabs(textPane.text)
    }

    def turnElasticTabstopsOff = {
      elasticToggle.text = "Elastic off"
      setFont(textPane, currentSettings.nonElasticFont)
      textPane.peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(new DocumentFilter)
      textPane.text = tabsToSpaces(textPane.text, currentSettings.nofIndentSpaces)
    }

    reactions += {
      case ButtonClicked(component) if component == elasticToggle =>
        elasticToggle.selected match {
          case true => turnElasticTabstopsOn
          case false => turnElasticTabstopsOff
        }
      case ButtonClicked(component) if component == settingsToggle =>
        settingsPanel.visible = settingsToggle.selected
      case ButtonClicked(component) if component == saveAndApplySettingsButton => {
        currentSettings = Settings.saveAndParse(settingsTextPane.text)

        if (elasticToggle.selected) {
          setFont(textPane, currentSettings.elasticFont)
          setElasticTabstopsDocFilter(textPane, currentSettings.elasticFont, onTextPaneChangeSetModified)
          textPane.text = textPane.text // force update of tabstop positions
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        } else {
          setFont(textPane, currentSettings.nonElasticFont)
        }

        setFont(settingsTextPane, currentSettings.elasticFont)
        setElasticTabstopsDocFilter(settingsTextPane, currentSettings.elasticFont)
        settingsTextPane.text = settingsTextPane.text // force update of tabstop positions
      }
      case ButtonClicked(component) if component == revertToDefaultSettingsButton =>
        settingsTextPane.text = Settings.defaultSettingsText
    }
  }

}
