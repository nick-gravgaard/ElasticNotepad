import java.awt.{Canvas, Font, GraphicsEnvironment}
import java.nio.file.Files
import scala.io.Source
import scala.swing.Dialog
import scala.util.{Failure, Try}

import fileHandling.{createAppDir, loadTextFile, saveTextFile, settingsFilePath}

package object settings:

  case class FontInfo(name: String, size: Int):
    override def toString: String = s""""$name", $size"""

  enum Theme:
    case Light, Dark
  object Theme:
    def fromString(s: String): Theme =
      s match
        case "Light" => Light
        case _ => Dark

  case class SettingText(key: String, description: String)

  case class Setting[A](value: A, text: SettingText):
    override def toString: String = s"${text.key}:\t$value\t| ${text.description}"

  case class Settings(
    elasticFont: Setting[FontInfo] = Setting[FontInfo](bestAvailableElasticFont, SettingText("Elastic font", "Used when elastic tabstops is on (can be proportional)")),
    nonElasticFont: Setting[FontInfo] = Setting[FontInfo](bestAvailableNonElasticFont, SettingText("Non-elastic font", "Used when elastic tabstops is off (monospaced is best)")),
    emptyColumnWidth: Setting[Double] = Setting[Double](1.8, SettingText("Empty column width", "Measured in multiples of line height (ems)")),
    columnPadding: Setting[Double] = Setting[Double](0.625, SettingText("Column padding", "Measured in multiples of line height (ems)")),
    nonElasticTabSize: Setting[Int] = Setting[Int](4, SettingText("Non-elastic tab size", "The indent size in non-elastic files")),
    filesAreNonElastic: Setting[Boolean] = Setting[Boolean](true, SettingText("Files on disk are non-elastic", "Convert to elastic tabstops on load, save as non-elastic")),
    theme: Setting[Theme] = Setting[Theme](Theme.Dark, SettingText("Theme", "\"Light\" or \"Dark\". Restart to take effect")),
    scale: Setting[Float] = Setting[Float](1.25, SettingText("Scale", "Multiplier to scale UI elements by. Restart to take effect"))
  )

  object Settings:
    private val preferredElasticFonts = List(
      FontInfo("Merriweather", 19),
      FontInfo("Palatino", 20),
      FontInfo("Palatino Linotype", 20),
      FontInfo("URW Palladio L", 20),
      FontInfo("Georgia", 20)
    )
    private val preferredNonElasticFonts = List(
      FontInfo("Inconsolata", 23),
      FontInfo("DejaVu Sans Mono", 20),
      FontInfo("Consolas", 20),
      FontInfo("Menlo", 20),
      FontInfo("Courier New", 20)
    )

    private val fallbackElasticFont = FontInfo("Serif", 20)
    private val fallbackNonElasticFont = FontInfo("Monospaced", 20)

    private val bestAvailableElasticFont = getBestAvailableFont(preferredElasticFonts, fallbackElasticFont)
    private val bestAvailableNonElasticFont = getBestAvailableFont(preferredNonElasticFonts, fallbackNonElasticFont)

    val defaults: Settings = Settings()

    def getBestAvailableFont(preferredFonts: List[FontInfo], fallbackFont: FontInfo): FontInfo =
      val availableFontNames = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames()
      preferredFonts.find(pf => availableFontNames.contains(pf.name)).getOrElse(fallbackFont)

    val defaultSettingsComment = "# Default settings (delete leading '>' to override)\n"
    val missingSettingsComment = "# The following settings are missing (delete leading '>' to override)\n"

    def defaultSettingsText: String =
      defaultSettingsComment + defaults.productIterator.map(">" + _.toString + "\n").mkString

    def removeTrailingNewline(text: String): String =
      if text.lastOption.contains('\n') then text.dropRight(1) else text

    def load: (Settings, String) =
      createAppDir()

      if Files.exists(settingsFilePath) then
        loadTextFile(settingsFilePath) match
          case Right(fileContents) =>
            val updatedFileContents = addMissingSettingsToText(fileContents)
            (fromString(updatedFileContents), updatedFileContents)
          case Left(errorMessage) =>
            Dialog.showMessage(null, errorMessage)
            (defaults, defaultSettingsText)
      else
        Try(Files.createFile(settingsFilePath)) recoverWith {
          case exception =>
            Dialog.showMessage(null, exception.getMessage)
            Failure(exception)
        }
        saveTextFile(defaultSettingsText, settingsFilePath)
        (defaults, defaultSettingsText)

    def saveAndParse(text: String): Settings =
      createAppDir()
      saveTextFile(text, settingsFilePath)
      fromString(text)

    def checkFontExists(fontName: String): Boolean =
      val g = GraphicsEnvironment.getLocalGraphicsEnvironment
      val fonts = g.getAvailableFontFamilyNames
      fonts contains fontName

    def getFont(m: Map[String, String], key: String): FontInfo =
      val backupFont = if key == defaults.nonElasticFont.text.key then bestAvailableNonElasticFont else bestAvailableElasticFont
      m.get(key) match
        case Some(value) =>
          val parts = value.split(',')
          val fontName = parts.headOption
            .flatMap(fn =>
              val strippedFontName: String = fn.strip.stripPrefix("\"").stripSuffix("\"")
              Option.when(checkFontExists(strippedFontName))(strippedFontName))
            .getOrElse(backupFont.name)
          val fontSize = parts.lift(1)
            .flatMap(fs => Try(fs.strip.toInt).toOption)
            .getOrElse(backupFont.size)
          FontInfo(fontName, fontSize)
        case None => backupFont

    def textToMaps(text: String): (Map[String, String], Map[String, String]) =
      val (active, inactive) = text.split('\n').toList
        .map(_.takeWhile(c => (c != '#') && (c != '|')))
        .map(_.strip)
        .filter(_.nonEmpty)
        .map(_.span(_ != ':'))
        .map { case (key, rest) => (key.stripTrailing, rest.stripPrefix(":").stripLeading) }
        .partition { !_._1.headOption.contains('>') }
      (active.toMap, inactive.map(kv => (kv._1.stripPrefix(">").stripLeading, kv._2)).toMap)

    def addMissingSettingsToText(text: String): String =
      val (active, inactive) = textToMaps(text)
      val missingSettings = defaults.productIterator.collect {
        case s: Setting[_] if !active.contains(s.text.key) && !inactive.contains(s.text.key) => s">${s.toString}"
      }.toList
      val leadingNewlines = if text.isEmpty then "" else "\n\n"
      text.strip +
        (if missingSettings.nonEmpty then
          leadingNewlines + missingSettingsComment + missingSettings.map(_ + '\n').mkString
        else
          "")

    def fromString(text: String): Settings =
      val (m, _) = textToMaps(text)
      Settings(
        Setting[FontInfo](
          getFont(m, defaults.elasticFont.text.key),
          defaults.elasticFont.text
        ),
        Setting[FontInfo](
          getFont(m, defaults.nonElasticFont.text.key),
          defaults.nonElasticFont.text
        ),
        Setting[Double](
          m.get(defaults.emptyColumnWidth.text.key)
            .flatMap(i => Try(i.toDouble).toOption)
            .getOrElse(defaults.emptyColumnWidth.value),
          defaults.emptyColumnWidth.text
        ),
        Setting[Double](
          m.get(defaults.columnPadding.text.key)
            .flatMap(i => Try(i.toDouble).toOption)
            .getOrElse(defaults.columnPadding.value),
          defaults.columnPadding.text
        ),
        Setting[Int](
          m.get(defaults.nonElasticTabSize.text.key)
            .flatMap(i => Try(i.toInt).toOption)
            .getOrElse(defaults.nonElasticTabSize.value),
          defaults.nonElasticTabSize.text
        ),
        Setting[Boolean](
          m.get(defaults.filesAreNonElastic.text.key)
            .flatMap(i => Try(i.toBoolean).toOption)
            .getOrElse(defaults.filesAreNonElastic.value),
          defaults.filesAreNonElastic.text
        ),
        Setting[Theme](
          m.get(defaults.theme.text.key)
            .flatMap(i => Try(Theme.fromString(i)).toOption)
            .getOrElse(defaults.theme.value),
          defaults.theme.text
        ),
        Setting[Float] (
          m.get (defaults.scale.text.key)
            .flatMap(i => Try(i.toFloat).toOption)
            .getOrElse(defaults.scale.value),
          defaults.scale.text
        )
      )
