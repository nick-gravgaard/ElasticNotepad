import java.awt.{Canvas, Color, Dimension, Font, FontMetrics}
import java.awt.Event.{CTRL_MASK, SHIFT_MASK}
import java.awt.event.KeyEvent.{VK_N, VK_O, VK_S}
import javax.swing.{KeyStroke, UIManager, WindowConstants}
import javax.swing.undo.{CannotRedoException, CannotUndoException, UndoManager}
import scala.swing.BorderPanel.Position.{Center, North}
import scala.swing.Dialog.Result
import scala.swing.event.ButtonClicked
import scala.swing.FlowPanel.Alignment.Left
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Dialog, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, Orientation, ScrollPane, Separator, SimpleSwingApplication, ToggleButton}

import com.bulenkov.darcula.DarculaLaf

import elasticTabstops.{spacesToTabs, tabsToSpaces}
import fileHandling.{chooseAndLoadFile, loadScratchFile, saveFile, saveFileAs, scratchFilePath}
import settings.{FontCC, Settings}
import textPanes.{EditorTextPane, ElasticTextPane}


object ElasticNotepad extends SimpleSwingApplication {

  var (currentSettings, currentSettingsText) = Settings.load

  UIManager.setLookAndFeel(new DarculaLaf)

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

  def top = new MainFrame {
    preferredSize = new Dimension(1536, 1024)
    maximize

    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    override def closeOperation() = {
      if (!textPane.modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to close this window?") == Result.Ok) {
        dispose()
      }
    }

    def scratchFileAction(): Action = {
      val action = Action("Open scratch file") {
        if (textPane.currentPath != scratchFilePath.toString && (!textPane.modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to switch to the scratch file?") == Result.Ok)) {
          textPane.currentPath = scratchFilePath.toString
          textPane.setNewText(if (currentSettings.filesAreNonElastic) spacesToTabs(loadScratchFile) else loadScratchFile)
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_N, CTRL_MASK))
      action
    }

    def loadFileAction(): Action = {
      val action = Action("Open...") {
        if (!textPane.modified || Dialog.showConfirmation(message = "There are unsaved changes. Are you sure you want to open another file?") == Result.Ok) {
          chooseAndLoadFile foreach { case (loadedText, path) =>
            textPane.currentPath = path
            textPane.setNewText(if (currentSettings.filesAreNonElastic) spacesToTabs(loadedText) else loadedText)
          }
        }
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_O, CTRL_MASK))
      action
    }

    def saveFileAction(): Action = {
      val action = Action("Save") {
        val textToSave = if (currentSettings.filesAreNonElastic) tabsToSpaces(textPane.text, currentSettings.nonElasticTabSize) else textPane.text
        saveFile(textToSave, textPane.currentPath)
        textPane.modified = false
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_S, CTRL_MASK))
      action
    }

    def saveFileAsAction(): Action = {
      val action = Action("Save as...") {
        val textToSave = if (currentSettings.filesAreNonElastic) tabsToSpaces(textPane.text, currentSettings.nonElasticTabSize) else textPane.text
        saveFileAs(textToSave) foreach { path =>
          textPane.currentPath = path
          textPane.modified = false
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

    val textPane = new EditorTextPane(
      new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size),
      currentSettings.emptyColumnWidth, currentSettings.minGapBetweenText,
      new Font(currentSettings.nonElasticFont.name, Font.PLAIN, currentSettings.nonElasticFont.size),
      currentSettings.nonElasticTabSize, scratchFilePath.toString
    ) {
      background = new Color(43, 43, 43)  // taken from Intellij IDEA
    }
    textPane.setNewText(if (currentSettings.filesAreNonElastic) spacesToTabs(loadScratchFile) else loadScratchFile)

    val elasticToggle = new ToggleButton { text = "Elastic on"; selected = true }
    val settingsToggle = new ToggleButton { text = "Settings"; selected = false }
    val toolbarPanel = new FlowPanel(Left)(elasticToggle, settingsToggle)
    val settingsTextPane = new ElasticTextPane(
      new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size),
      currentSettings.emptyColumnWidth, currentSettings.minGapBetweenText
    )
    settingsTextPane.setNewText(currentSettingsText)

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

    reactions += {
      case ButtonClicked(component) if component == elasticToggle =>
        textPane.elastic = elasticToggle.selected
        elasticToggle.text = textPane.elastic match {
          case true => "Elastic on"
          case false => "Elastic off"
        }
      case ButtonClicked(component) if component == settingsToggle =>
        settingsPanel.visible = settingsToggle.selected
      case ButtonClicked(component) if component == saveAndApplySettingsButton => {
        currentSettings = Settings.saveAndParse(settingsTextPane.text)
        textPane.changeSettings(
          new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size),
          currentSettings.emptyColumnWidth, currentSettings.minGapBetweenText,
          new Font(currentSettings.nonElasticFont.name, Font.PLAIN, currentSettings.nonElasticFont.size),
          currentSettings.nonElasticTabSize
        )
        settingsTextPane.changeSettings(
          new Font(currentSettings.elasticFont.name, Font.PLAIN, currentSettings.elasticFont.size),
          currentSettings.emptyColumnWidth, currentSettings.minGapBetweenText
        )
      }
      case ButtonClicked(component) if component == revertToDefaultSettingsButton =>
        settingsTextPane.text = Settings.defaultSettingsText
    }

    textPane.updateWindowTitle
  }

}
