import java.awt.{Canvas, Font, GraphicsEnvironment}
import java.nio.file.Files
import scala.io.Source
import scala.swing.Dialog
import scala.util.{Failure, Try}

import fileHandling.{createAppDir, loadTextFile, saveTextFile, settingsFilePath}


package object settings {

  case class FontCC(name: String, size: Int) {
    override def toString: String = s""""${name}", ${size}"""
  }

  case class Settings(elasticFont: FontCC,
                      nonElasticFont: FontCC,
                      emptyColumnWidth: Double,
                      columnPadding: Double,
                      nonElasticTabSize: Int,
                      filesAreNonElastic: Boolean)

  object Settings {
    private val preferredElasticFonts = List(
      FontCC("Merriweather", 22),
      FontCC("Palatino", 25),
      FontCC("Palatino Linotype", 25),
      FontCC("URW Palladio L", 25),
      FontCC("Georgia", 24)
    )
    private val preferredNonElasticFonts = List(
      FontCC("Inconsolata", 26),
      FontCC("DejaVu Sans Mono", 23),
      FontCC("Consolas", 23),
      FontCC("Menlo", 23),
      FontCC("Courier New", 24)
    )

    private val fallbackElasticFont = FontCC("Serif", 24)
    private val fallbackNonElasticFont = FontCC("Monospaced", 24)

    private val bestAvailableElasticFont = getBestAvailableFont(preferredElasticFonts, fallbackElasticFont)
    private val bestAvailableNonElasticFont = getBestAvailableFont(preferredNonElasticFonts, fallbackNonElasticFont)

    def defaults = Settings(bestAvailableElasticFont, bestAvailableNonElasticFont, 1.8, 0.625, 4, true)

    private val elasticFontText = ("Elastic font", "Used when elastic tabstops is on (can be proportional)")
    private val nonElasticFontText = ("Non-elastic font", "Used when elastic tabstops is off (monospaced is best)")
    private val emptyColumnWidthText = ("Empty column width", "Measured in multiples of line height (ems)")
    private val columnPaddingText = ("Column padding", "Measured in multiples of line height (ems)")
    private val nonElasticTabSizeText = ("Non-elastic tab size", "The indent size in non-elastic files")
    private val filesAreNonElasticText = ("Files on disk are non-elastic", "Convert to elastic tabstops when loading (and save as non-elastic)")

    def getBestAvailableFont(preferredFonts: List[FontCC], fallbackFont: FontCC): FontCC = {
      val availableFontNames = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames()
      preferredFonts.find(pf => availableFontNames.contains(pf.name)).getOrElse(fallbackFont)
    }

    def defaultSettingsText: String = {
      val cellsPerLine = List(
        (defaults.elasticFont.toString, elasticFontText),
        (defaults.nonElasticFont.toString, nonElasticFontText),
        (defaults.emptyColumnWidth.toString, emptyColumnWidthText),
        (defaults.columnPadding.toString, columnPaddingText),
        (defaults.nonElasticTabSize.toString, nonElasticTabSizeText),
        (defaults.filesAreNonElastic.toString, filesAreNonElasticText)
      )
      cellsPerLine.map { case (value, (key, description)) => s"$key:\t$value\t| $description" }.mkString("\n")
    }

    def load: (Settings, String) = {
      createAppDir

      Files.exists(settingsFilePath) match {
        case false => {
          Try(Files.createFile(settingsFilePath)) recoverWith {
            case exception => {
              Dialog.showMessage(null, exception.getMessage)
              Failure(exception)
            }
          }
          saveTextFile(defaultSettingsText, settingsFilePath)
          (defaults, defaultSettingsText)
        }
        case true => {
          loadTextFile(settingsFilePath) match {
            case Right(fileContents) => {
              (fromString(fileContents), fileContents)
            }
            case Left(errorMessage) => {
              Dialog.showMessage(null, errorMessage)
              (defaults, defaultSettingsText)
            }
          }
        }
      }
    }

    def saveAndParse(text: String): Settings = {
      createAppDir
      saveTextFile(text, settingsFilePath)
      fromString(text)
    }

    def checkFontExists(fontName: String): Boolean = {
      val g = GraphicsEnvironment.getLocalGraphicsEnvironment
      val fonts = g.getAvailableFontFamilyNames
      fonts contains fontName
    }

    def getFont(m: Map[String, String], key: String): FontCC = {
      val backupFont = if (key == nonElasticFontText._1) fallbackNonElasticFont else fallbackElasticFont
      m.get(key) match {
        case Some(value) => {
          val parts = value.split(',')
          parts.length match {
            case 1 => FontCC(
              {
                val fontName = parts(0).trim().stripPrefix("\"").stripSuffix("\"")
                if (checkFontExists(fontName)) fontName else backupFont.name
              },
              backupFont.size
            )
            case 2 => {
              val fontName = parts(0).trim().stripPrefix("\"").stripSuffix("\"")
              if (checkFontExists(fontName))
                FontCC(fontName, Try(parts(1).trim().toInt).toOption.getOrElse(backupFont.size))
              else
                backupFont
            }
            case _ => backupFont
          }
        }
        case None => backupFont
      }
    }

    def fromString(text: String): Settings = {
      val m = text.split('\n').map { line =>
        val parts = line.take(line.indexOf('|')).split(':')
        val key = parts(0).trim
        val value = parts(1).trim
        key -> value
      }.toMap
      Settings(
        getFont(m, elasticFontText._1),
        getFont(m, nonElasticFontText._1),
        m.get(emptyColumnWidthText._1).flatMap(i => Try(i.toDouble).toOption).getOrElse(defaults.emptyColumnWidth),
        m.get(columnPaddingText._1).flatMap(i => Try(i.toDouble).toOption).getOrElse(defaults.columnPadding),
        m.get(nonElasticTabSizeText._1).flatMap(i => Try(i.toInt).toOption).getOrElse(defaults.nonElasticTabSize),
        m.get(filesAreNonElasticText._1).flatMap(i => Try(i.toBoolean).toOption).getOrElse(defaults.filesAreNonElastic)
      )
    }

  }

}
