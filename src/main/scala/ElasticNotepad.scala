import java.awt.{Canvas, Dimension, Font, FontMetrics}
import java.awt.Event.{CTRL_MASK, SHIFT_MASK}
import java.awt.event.KeyEvent.{VK_N, VK_O, VK_S, VK_Z}
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.text._
import javax.swing.{KeyStroke, UIManager, WindowConstants}
import javax.swing.event.{DocumentEvent, UndoableEditEvent, UndoableEditListener}
import javax.swing.undo.{CannotRedoException, CannotUndoException, UndoManager}

import buildInfo.BuildInfo.{name => appName, version => appVersion}
import elasticTabstops.{calcTabstopPositions, spacesToTabs, tabsToSpaces}
import fileHandling.{chooseAndLoadFile, loadScratchFile, saveFile, saveFileAs, scratchFilePath}
import settings.{FontCC, Settings}

import scala.swing.BorderPanel.Position.{Center, North}
import scala.swing.Dialog.Result
import scala.swing.FlowPanel.Alignment.Left
import scala.swing.event.{ButtonClicked, Key, KeyPressed}
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Dialog, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, Orientation, ScrollPane, Separator, SimpleSwingApplication, TextPane, ToggleButton}

object ElasticNotepad extends SimpleSwingApplication {

  var (currentSettings, currentSettingsText) = Settings.load
  var currentPath = scratchFilePath.toString
  var modified = false

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

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

  scaleUiFonts(1.5f)

  def makeWindowTitleText(path: String): String = {
    s"${if (modified) "* " else ""}$path - $appName v$appVersion"
  }

  def getRecalcRange(textPerLine: List[String], startLineNum: Int, nofLines: Int): (Int, Int) = {
    val indexedLines = textPerLine.zipWithIndex
    val recalcStart = indexedLines.take(startLineNum).reverse.find(_._1.count(_ == '\t') == 0) match {
      case None => 0
      case Some((_, lineNum)) => lineNum
    }
    val recalcEnd = indexedLines.drop(startLineNum + nofLines).find(_._1.count(_ == '\t') == 0) match {
      case None => indexedLines.length
      case Some((_, lineNum)) => lineNum + 1
    }
    val recalcLength = recalcEnd - recalcStart
    (recalcStart, recalcLength)
  }

  def alignTabstops(doc: StyledDocument, fm: FontMetrics, startAndLength: Option[(Int, Int)] = None): Unit = {
    val section = doc.getDefaultRootElement

    val allElements = (for (l <- 0 until section.getElementCount) yield section.getElement(l)).toList
    val allTextPerLine = for (el <- allElements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)

    val (recalcStart, recalcLength) = startAndLength match {
      case None => (0, allTextPerLine.length)
      case Some((lineNum, nofLines)) => {
        getRecalcRange(allTextPerLine, lineNum, nofLines)
      }
    }

    val elements = allElements.drop(recalcStart).take(recalcLength)
    val textPerLine = for (el <- elements) yield doc.getText(el.getStartOffset, el.getEndOffset - el.getStartOffset)
    val cellsPerLine = textPerLine.map(_.split('\t').toList)
    def calcCellWidth(text: String): Int = math.max(fm.stringWidth(text), currentSettings.emptyColumnWidthMinusGapPx) + currentSettings.minGapBetweenTextPx
    for ((tabstopPositionsThisLine, element) <- calcTabstopPositions(cellsPerLine, calcCellWidth).zip(elements)) {
      val tabStops = tabstopPositionsThisLine.map(new TabStop(_))
      val attributes = new SimpleAttributeSet()
      StyleConstants.setTabSet(attributes, new TabSet(tabStops.toArray))
      val length = element.getEndOffset - element.getStartOffset
      doc.setParagraphAttributes(element.getStartOffset, length, attributes, false)
    }
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
        if (!modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to open another file?") == Result.Ok) {
          chooseAndLoadFile(currentSettings.filesEndWithNewline) foreach { case (loadedText, path) =>
            textPane.text = if (currentSettings.filesAreNonElastic) spacesToTabs(loadedText) else loadedText
            currentPath = path
            modified = false
            setWindowTitle(makeWindowTitleText(currentPath))
          }
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_O, CTRL_MASK))
      action
    }

    def saveFileAction(): Action = {
      val action = Action("Save") {
        val textToSave = if (currentSettings.filesAreNonElastic) tabsToSpaces(textPane.text, currentSettings.nonElasticTabSize) else textPane.text
        saveFile(textToSave, currentSettings.filesEndWithNewline, currentPath)
        modified = false
        setWindowTitle(makeWindowTitleText(currentPath))
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_S, CTRL_MASK))
      action
    }

    def saveFileAsAction(): Action = {
      val action = Action("Save as...") {
        val textToSave = if (currentSettings.filesAreNonElastic) tabsToSpaces(textPane.text, currentSettings.nonElasticTabSize) else textPane.text
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

    def setElasticTabstopsDocFilter(textPane: TextPane, fontMetrics: FontMetrics, onChange: () => Unit = { () => Unit }) = {

      object ElasticTabstopsDocFilter extends DocumentFilter {
        override def insertString(fb: FilterBypass, offset: Int, string: String, attributes: AttributeSet) {
          super.insertString(fb, offset, string, attributes)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          val lineNum = doc.getDefaultRootElement.getElementIndex(offset)
          val nofLines = string.count(_ == '\n') + 1
          alignTabstops(doc, fontMetrics, Some(lineNum, nofLines))
        }

        override def remove(fb: FilterBypass, offset: Int, length: Int) {
          super.remove(fb, offset, length)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          val lineNum = doc.getDefaultRootElement.getElementIndex(offset)
          val nofLines = 1
          alignTabstops(doc, fontMetrics, Some(lineNum, nofLines))
        }

        override def replace(fb: FilterBypass, offset: Int, length: Int, string: String, attributes: AttributeSet) {
          super.replace(fb, offset, length, string, attributes)
          onChange()
          val doc = fb.getDocument.asInstanceOf[StyledDocument]
          val lineNum = doc.getDefaultRootElement.getElementIndex(offset)
          val nofLines = string.count(_ == '\n') + 1
          alignTabstops(doc, fontMetrics, Some(lineNum, nofLines))
        }
      }
      textPane.peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(ElasticTabstopsDocFilter)
    }

    val textPane = new TextPane { font = new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size) }
    var elasticFontMetrics = new Canvas().getFontMetrics(new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size))
    setElasticTabstopsDocFilter(textPane, elasticFontMetrics, onTextPaneChangeSetModified)
    textPane.text = loadScratchFile
    modified = false
    setWindowTitle(makeWindowTitleText(currentPath))

    val undoManager = new UndoManager
    val doc = textPane.peer.getDocument
    doc.addUndoableEditListener((evt: UndoableEditEvent) => {
      val eventType = evt.getEdit.asInstanceOf[DocumentEvent].getType
      if (eventType != DocumentEvent.EventType.CHANGE) {
        // don't allow undoing of style changes (so we ignore tabstop changes)
        undoManager.addEdit(evt.getEdit)
      }
    })

    def undoAction(): Action = {
      val action = Action("Undo") {
        try {
          if (undoManager.canUndo) {
            undoManager.undo()
            alignTabstops(doc.asInstanceOf[StyledDocument], elasticFontMetrics)
          }
        }
        catch {
          case e: CannotUndoException =>
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_Z, CTRL_MASK))
      action
    }

    def redoAction(): Action = {
      val action = Action("Redo") {
        try {
          if (undoManager.canRedo) {
            undoManager.redo()
            alignTabstops(doc.asInstanceOf[StyledDocument], elasticFontMetrics)
          }
        }
        catch {
          case e: CannotRedoException =>
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_Z, CTRL_MASK | SHIFT_MASK))
      action
    }

    val elasticToggle = new ToggleButton { text = "Elastic on"; selected = true }
    val settingsToggle = new ToggleButton { text = "Settings"; selected = false }
    val toolbarPanel = new FlowPanel(Left)(elasticToggle, settingsToggle)
    val settingsTextPane = new TextPane { font = new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size) }
    setElasticTabstopsDocFilter(settingsTextPane, elasticFontMetrics)
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

    listenTo(textPane.keys)
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
      setElasticTabstopsDocFilter(textPane, elasticFontMetrics, onTextPaneChangeSetModified)
      textPane.text = spacesToTabs(textPane.text)
    }

    def turnElasticTabstopsOff = {
      elasticToggle.text = "Elastic off"
      setFont(textPane, currentSettings.nonElasticFont)
      textPane.peer.getDocument().asInstanceOf[AbstractDocument].setDocumentFilter(new DocumentFilter)
      textPane.text = tabsToSpaces(textPane.text, currentSettings.nonElasticTabSize)
    }

    reactions += {
      case kp @ KeyPressed(_, Key.Z, _, _) => {
        if (kp.peer.isControlDown()) {
          if (kp.peer.isShiftDown()) {
            redoAction.apply
          } else {
            undoAction.apply
          }
        }
      }
      case ButtonClicked(component) if component == elasticToggle =>
        elasticToggle.selected match {
          case true => turnElasticTabstopsOn
          case false => turnElasticTabstopsOff
        }
      case ButtonClicked(component) if component == settingsToggle =>
        settingsPanel.visible = settingsToggle.selected
      case ButtonClicked(component) if component == saveAndApplySettingsButton => {
        currentSettings = Settings.saveAndParse(settingsTextPane.text)
        elasticFontMetrics = new Canvas().getFontMetrics(new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size))

        if (elasticToggle.selected) {
          setFont(textPane, currentSettings.elasticFont)
          setElasticTabstopsDocFilter(textPane, elasticFontMetrics, onTextPaneChangeSetModified)
          textPane.text = textPane.text // force update of tabstop positions
          modified = false
          setWindowTitle(makeWindowTitleText(currentPath))
        } else {
          setFont(textPane, currentSettings.nonElasticFont)
        }

        setFont(settingsTextPane, currentSettings.elasticFont)
        setElasticTabstopsDocFilter(settingsTextPane, elasticFontMetrics)
        settingsTextPane.text = settingsTextPane.text // force update of tabstop positions
      }
      case ButtonClicked(component) if component == revertToDefaultSettingsButton =>
        settingsTextPane.text = Settings.defaultSettingsText
    }
  }

}
