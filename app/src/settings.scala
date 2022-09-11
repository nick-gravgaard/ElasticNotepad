import java.awt.{Canvas, Font, GraphicsEnvironment}
import java.nio.file.Files
import scala.io.Source
import scala.swing.Dialog
import scala.util.{Failure, Try}

import fileHandling.{createAppDir, loadTextFile, saveTextFile, settingsFilePath}

package object settings:

  case class FontInfo(name: String, size: Int):
    override def toString: String = s""""${name}", ${size}"""

  enum Theme:
    case Light, Dark
  object Theme:
    def fromString(s: String) =
      s match
        case "Light" => Light
        case _ => Dark

  case class SettingText(key: String, description: String)

  case class Setting[A](value: A, text: SettingText):
    override def toString: String = s"${text.key}:\t$value\t| ${text.description}"

  case class Settings(
    elasticFont: Setting[FontInfo],
    nonElasticFont: Setting[FontInfo],
    emptyColumnWidth: Setting[Double],
    columnPadding: Setting[Double],
    nonElasticTabSize: Setting[Int],
    filesAreNonElastic: Setting[Boolean],
    theme: Setting[Theme],
    scale: Setting[Float]
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

    def defaults = Settings(
      Setting[FontInfo](bestAvailableElasticFont, SettingText("Elastic font", "Used when elastic tabstops is on (can be proportional)")),
      Setting[FontInfo](bestAvailableNonElasticFont, SettingText("Non-elastic font", "Used when elastic tabstops is off (monospaced is best)")),
      Setting[Double](1.8, SettingText("Empty column width", "Measured in multiples of line height (ems)")),
      Setting[Double](0.625, SettingText("Column padding", "Measured in multiples of line height (ems)")),
      Setting[Int](4, SettingText("Non-elastic tab size", "The indent size in non-elastic files")),
      Setting[Boolean](true, SettingText("Files on disk are non-elastic", "Convert to elastic tabstops when loading (and save as non-elastic)")),
      Setting[Theme](Theme.Dark, SettingText("Theme", "\"Light\" or \"Dark\". Restart to see any change take effect")),
      Setting[Float](1.25, SettingText("Scale", "Multiplier to scale UI elements by. Restart to see any change take effect"))
    )

    def getBestAvailableFont(preferredFonts: List[FontInfo], fallbackFont: FontInfo): FontInfo =
      val availableFontNames = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames()
      preferredFonts.find(pf => availableFontNames.contains(pf.name)).getOrElse(fallbackFont)

    def defaultSettingsText: String =
      val cellsPerLine = List(
        defaults.elasticFont.toString,
        defaults.nonElasticFont.toString,
        defaults.emptyColumnWidth.toString,
        defaults.columnPadding.toString,
        defaults.nonElasticTabSize.toString,
        defaults.filesAreNonElastic.toString,
        defaults.theme.toString,
        defaults.scale.toString
      )
      cellsPerLine.map(_.toString).mkString("\n")

    def load: (Settings, String) =
      createAppDir()

      Files.exists(settingsFilePath) match
        case false =>
          Try(Files.createFile(settingsFilePath)) recoverWith {
            case exception =>
              Dialog.showMessage(null, exception.getMessage)
              Failure(exception)
          }
          saveTextFile(defaultSettingsText, settingsFilePath)
          (defaults, defaultSettingsText)
        case true =>
          loadTextFile(settingsFilePath) match
            case Right(fileContents) =>
              (fromString(fileContents), fileContents)
            case Left(errorMessage) =>
              Dialog.showMessage(null, errorMessage)
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
      val backupFont = if key == defaults.nonElasticFont.text.key then fallbackNonElasticFont else fallbackElasticFont
      m.get(key) match
        case Some(value) =>
          val parts = value.split(',')
          parts.length match
            case 1 => FontInfo(
              {
                val fontName = parts(0).trim().stripPrefix("\"").stripSuffix("\"")
                if checkFontExists(fontName) then fontName else backupFont.name
              },
              backupFont.size
            )
            case 2 =>
              val fontName = parts(0).trim().stripPrefix("\"").stripSuffix("\"")
              if checkFontExists(fontName) then
                FontInfo(fontName, Try(parts(1).trim().toInt).toOption.getOrElse(backupFont.size))
              else
                backupFont
            case _ => backupFont
        case None => backupFont

    def fromString(text: String): Settings =
      val m = text.split('\n').map { line =>
        val parts = line.take(line.indexOf('|')).split(':')
        val key = parts(0).trim
        val value = parts(1).trim
        key -> value
      }.toMap
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
