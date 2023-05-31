import java.awt.{Canvas, Color, Dimension, Font, FontMetrics}
import java.awt.event.InputEvent.SHIFT_DOWN_MASK
import java.awt.event.KeyEvent.{VK_N, VK_O, VK_Q, VK_S}
import java.awt.Toolkit
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import javax.swing.{KeyStroke, UIManager, WindowConstants}
import javax.swing.undo.{CannotRedoException, CannotUndoException, UndoManager}
import scala.swing.BorderPanel.Position.{Center, North}
import scala.swing.Dialog.Result
import scala.swing.event.ButtonClicked
import scala.swing.FlowPanel.Alignment.Left
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Dialog, FlowPanel, MainFrame, Menu, MenuBar, MenuItem, Orientation, ScrollPane, Separator, SimpleSwingApplication, ToggleButton}
import com.formdev.flatlaf.{FlatDarculaLaf, FlatIntelliJLaf}
import elasticTabstops.{spacesToTabs, tabsToSpaces}
import fileHandling.{chooseAndLoadTextFile, loadScratchFile, saveTextFile, saveTextFileAs, scratchFilePath}
import settings.Settings.removeTrailingNewline
import settings.{FontInfo, Settings, Theme}
import textPanes.{EditorTextPane, ElasticTextPane}

object ElasticNotepad extends SimpleSwingApplication:

  var maybePath: Option[Path] = None

  override def startup(args: Array[String]) =
    val minimumJavaVersion = 14
    val isCompatibleJavaVersion = (for
      javaVersion       <- Option(System.getProperty("java.version"))
      mainVersionNumber <- javaVersion.takeWhile(_.isDigit).toIntOption
    yield mainVersionNumber >= minimumJavaVersion).getOrElse(false)
    if !isCompatibleJavaVersion then
      println(s"Error: This program requires Java $minimumJavaVersion or later")
      System.exit(1)

    maybePath = args.toList match
      case Nil => None
      case pathText :: Nil =>
        val path = Paths.get(pathText)
        if Files.exists(path) then
          Some(path)
        else
          println(s"""Error: file "$path" does not exist""")
          System.exit(1)
          None
      case _ =>
        println("Error: 0 or 1 argument (filename) expected")
        System.exit(1)
        None

    super.startup(Array[String]())

  if System.getProperty("os.name") == "Mac OS X" then
    System.setProperty("apple.laf.useScreenMenuBar", "true")
  val shortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMaskEx

  var (currentSettings, currentSettingsText) = Settings.load

  def setLookAndFeel(theme: Theme) =
    UIManager.setLookAndFeel(
      theme match
        case Theme.Light => new FlatIntelliJLaf
        case Theme.Dark => new FlatDarculaLaf
    )

  setLookAndFeel(currentSettings.theme.value)

  def scaleUiFonts(multiplier: Float) =
    UIManager.getLookAndFeelDefaults.keySet.asScala.foreach { key =>
      val keyName = key.toString
      if keyName.endsWith(".font") then
        val font = UIManager.getFont(keyName)
        val biggerFont = font.deriveFont(multiplier * font.getSize2D)
        UIManager.put(keyName, biggerFont)
    }

  scaleUiFonts(currentSettings.scale.value)

  def top = new MainFrame:
    preferredSize = new Dimension(1536, 1024)
    maximize()

    peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)

    override def closeOperation() =
      val msg = "There are unsaved changes. Are you sure you want to close this window?"
      if !textPane.modified || Dialog.showConfirmation(message = msg) == Result.Ok then
        dispose()

    def scratchFileAction(): Action =
      val action = Action("Open scratch file") {
        textPane.openScratchFile(scratchFilePath, currentSettings)
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_N, shortcutKeyMask))
      action

    def loadFileAction(): Action =
      val action = Action("Open...") {
        textPane.openFile(currentSettings)
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_O, shortcutKeyMask))
      action

    def saveFileAction(): Action =
      val action = Action("Save") {
        textPane.saveFile(currentSettings)
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_S, shortcutKeyMask))
      action

    def saveFileAsAction(): Action =
      val action = Action("Save as...") {
        textPane.saveFileAs(currentSettings)
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_S, shortcutKeyMask | SHIFT_DOWN_MASK))
      action

    def quitAction(): Action =
      val action = Action("Quit") {
        closeOperation()
      }
      action.accelerator = Some(KeyStroke.getKeyStroke(VK_Q, shortcutKeyMask))
      action

    menuBar = new MenuBar:
      contents += new Menu("File"):
        contents ++= List(
          new MenuItem(scratchFileAction()),
          new MenuItem(loadFileAction()),
          new Separator,
          new MenuItem(saveFileAction()),
          new MenuItem(saveFileAsAction()),
          new Separator,
          new MenuItem(quitAction())
        )

    val textPane = new EditorTextPane(
      new Font(currentSettings.elasticFont.value.name, Font.PLAIN, currentSettings.elasticFont.value.size),
      currentSettings.emptyColumnWidth.value, currentSettings.columnPadding.value,
      new Font(currentSettings.nonElasticFont.value.name, Font.PLAIN, currentSettings.nonElasticFont.value.size),
      currentSettings.nonElasticTabSize.value, currentSettings.filesAreNonElastic.value, maybePath
    ):
      if currentSettings.theme.value == Theme.Dark then
        background = new Color(43, 43, 43)  // taken from Intellij IDEA

    val elasticToggle = new ToggleButton:
      text = "Elastic on"
      selected = true
    val settingsToggle = new ToggleButton:
      text = "Settings"
      selected = false
    val toolbarPanel = new FlowPanel(Left)(elasticToggle, settingsToggle)
    val settingsTextPane = new ElasticTextPane(
      new Font(currentSettings.elasticFont.value.name, Font.PLAIN, currentSettings.elasticFont.value.size),
      currentSettings.emptyColumnWidth.value, currentSettings.columnPadding.value
    )
    settingsTextPane.setNewText(removeTrailingNewline(currentSettingsText))

    val saveAndApplySettingsButton = new Button("Save and apply")
    val revertToDefaultSettingsButton = new Button("Revert to defaults")
    val settingsToolbarPanel = new FlowPanel(Left)(saveAndApplySettingsButton, revertToDefaultSettingsButton)

    val settingsPanel = new BoxPanel(Orientation.Vertical):
      visible = false
      contents ++= List(new Separator, settingsTextPane, settingsToolbarPanel)

    val toolbarAndSettingsPanel = new BoxPanel(Orientation.Vertical):
      contents ++= List(toolbarPanel, settingsPanel)

    val scrollPane = new ScrollPane(textPane)
    scrollPane.verticalScrollBar.unitIncrement = 8

    contents = new BorderPanel:
      layout(toolbarAndSettingsPanel) = North
      layout(scrollPane) = Center

    listenTo(elasticToggle)
    listenTo(settingsToggle)
    listenTo(saveAndApplySettingsButton)
    listenTo(revertToDefaultSettingsButton)

    reactions += {
      case ButtonClicked(component) if component == elasticToggle =>
        textPane.elastic = elasticToggle.selected
        elasticToggle.text = "Elastic " + (if textPane.elastic then "on" else "off")
      case ButtonClicked(component) if component == settingsToggle =>
        settingsPanel.visible = settingsToggle.selected
      case ButtonClicked(component) if component == saveAndApplySettingsButton =>
        currentSettings = Settings.saveAndParse(settingsTextPane.text + '\n')
        textPane.changeSettings(
          new Font(currentSettings.elasticFont.value.name, Font.PLAIN, currentSettings.elasticFont.value.size),
          currentSettings.emptyColumnWidth.value, currentSettings.columnPadding.value,
          new Font(currentSettings.nonElasticFont.value.name, Font.PLAIN, currentSettings.nonElasticFont.value.size),
          currentSettings.nonElasticTabSize.value
        )
        settingsTextPane.changeSettings(
          new Font(currentSettings.elasticFont.value.name, Font.PLAIN, currentSettings.elasticFont.value.size),
          currentSettings.emptyColumnWidth.value, currentSettings.columnPadding.value
        )
      case ButtonClicked(component) if component == revertToDefaultSettingsButton =>
        settingsTextPane.text = removeTrailingNewline(Settings.defaultSettingsText)
    }

    textPane.updateWindowTitle()
