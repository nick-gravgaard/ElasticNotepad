import java.awt.{Canvas, Font, GraphicsEnvironment}
import java.nio.file.Files

import fileHandling.{createAppDir, loadFile, saveFile, settingsFilePath}

import scala.io.Source
import scala.swing.Dialog
import scala.util.{Failure, Try}

package object settings {

  case class FontCC(name: String, size: Int) {
    override def toString: String = s""""${name}", ${size}"""
  }

  case class Settings(elasticFont: FontCC,
                      nonElasticFont: FontCC,
                      emptyColumnWidth: Double,
                      minGapBetweenText: Double,
                      nonElasticTabSize: Int,
                      filesAreNonElastic: Boolean,
                      filesEndWithNewline: Boolean) {

    val (emptyColumnWidthMinusGapPx, minGapBetweenTextPx) = {
      val fontMetrics = new Canvas().getFontMetrics(new Font(elasticFont.name, Font.PLAIN, elasticFont.size))
      val em = fontMetrics.getHeight // more accurate than the font's point size (with Merriweather at least)
      val emptyColumnWidthPx = (emptyColumnWidth * em).toInt
      val minGapBetweenTextPx = (minGapBetweenText * em).toInt
      val emptyColumnWidthMinusGapPx = emptyColumnWidthPx - minGapBetweenTextPx
      (emptyColumnWidthMinusGapPx, minGapBetweenTextPx)
    }
  }

  object Settings {
    def defaults = Settings(FontCC("Merriweather", 25), FontCC("Droid Sans Mono", 23), 1.8, 0.625, 4, true, true)

    private val backupElasticFont = FontCC("Serif", 23)
    private val backupNonElasticFont = FontCC("Monospaced", 23)

    private val elasticFontText = ("Elastic font", "Used when elastic tabstops is on (can be proportional)")
    private val nonElasticFontText = ("Non-elastic font", "Used when elastic tabstops is off (monospaced is best)")
    private val emptyColumnWidthText = ("Empty column width", "Measured in multiples of line height (ems)")
    private val minGapBetweenTextText = ("Smallest gap between text", "Measured in multiples of line height (ems)")
    private val nonElasticTabSizeText = ("Non-elastic tab size", "The indent size in non-elastic files")
    private val filesAreNonElasticText = ("Files on disk are non-elastic", "Convert to elastic tabstops when loading (and save as non-elastic)")
    private val filesEndWithNewlineText = ("Exactly one newline at end of file", "Many tools need all lines to be terminated by a newline")

    def defaultSettingsText: String = {
      val cellsPerLine = List(
        (defaults.elasticFont.toString, elasticFontText),
        (defaults.nonElasticFont.toString, nonElasticFontText),
        (defaults.emptyColumnWidth.toString, emptyColumnWidthText),
        (defaults.minGapBetweenText.toString, minGapBetweenTextText),
        (defaults.nonElasticTabSize.toString, nonElasticTabSizeText),
        (defaults.filesAreNonElastic.toString, filesAreNonElasticText),
        (defaults.filesEndWithNewline.toString, filesEndWithNewlineText)
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
          saveFile(defaultSettingsText, true, settingsFilePath.toString)
          (defaults, defaultSettingsText)
        }
        case true => {
          loadFile(Source.fromFile(settingsFilePath.toString, "UTF-8"), true) match {
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
      saveFile(text, true, settingsFilePath.toString)
      fromString(text)
    }

    def checkFontExists(fontName: String): Boolean = {
      val g = GraphicsEnvironment.getLocalGraphicsEnvironment
      val fonts = g.getAvailableFontFamilyNames
      fonts contains fontName
    }

    def getFont(m: Map[String, String], key: String): FontCC = {
      val backupFont = if (key == nonElasticFontText._1) backupNonElasticFont else backupElasticFont
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
        m.get(minGapBetweenTextText._1).flatMap(i => Try(i.toDouble).toOption).getOrElse(defaults.minGapBetweenText),
        m.get(nonElasticTabSizeText._1).flatMap(i => Try(i.toInt).toOption).getOrElse(defaults.nonElasticTabSize),
        m.get(filesAreNonElasticText._1).flatMap(i => Try(i.toBoolean).toOption).getOrElse(defaults.filesAreNonElastic),
        m.get(filesEndWithNewlineText._1).flatMap(i => Try(i.toBoolean).toOption).getOrElse(defaults.filesEndWithNewline)
      )
    }

  }

}
