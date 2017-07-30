import java.awt.FileDialog
import java.io.{BufferedWriter, FileNotFoundException, FileOutputStream, IOException, OutputStreamWriter}
import java.nio.file.{Files, Path, Paths}

import scala.io.Source
import scala.swing.Dialog
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

package object fileHandling {

  val appDirPath: Path = Paths.get(s"${System.getProperty("user.home")}/.elasticnotepad")
  val scratchFilePath: Path = Paths.get(s"$appDirPath/scratch")
  val settingsFilePath: Path = Paths.get(s"$appDirPath/settings")

  def alwaysClose[A <: {def close(): Unit}, B] (closeable: A) (f: A => B): B = {
    try {
      f(closeable)
    } finally {
      try {
        closeable.close()
      } catch {
        case NonFatal(e) =>
          println("Error closing resource:")
          e.printStackTrace
      }
    }
  }

  def createAppDir = {
    if (!Files.exists(appDirPath)) {
      Try(Files.createDirectory(appDirPath)) recoverWith {
        case exception => {
          Dialog.showMessage(null, exception.getMessage)
          Failure(exception)
        }
      }
    }
  }

  def loadScratchFile: String = {
    createAppDir

    Files.exists(scratchFilePath) match {
      case false => {
        Try(Files.createFile(scratchFilePath)) recoverWith {
          case exception => {
            Dialog.showMessage(null, exception.getMessage)
            Failure(exception)
          }
        }
        saveFile(assets.InitialText, true, scratchFilePath.toString)
        assets.InitialText
      }
      case true => {
        loadFile(Source.fromFile(scratchFilePath.toString, "UTF-8"), true) match {
          case Right(fileContents) => {
            fileContents
          }
          case Left(errorMessage) => {
            Dialog.showMessage(null, errorMessage)
            assets.InitialText
          }
        }
      }
    }
  }


  def loadFile(fileSource: Source, trimNewlines: Boolean): Either[String, String] = {
    try {
      alwaysClose(fileSource) { handledFileSource =>
        val text = handledFileSource.getLines.mkString("\n")
        Right(if (trimNewlines) text.replaceAll("X+$", "") else text)
      }
    } catch {
      case e: FileNotFoundException => Left(s"Can't find ${fileSource.descr}\n\n[${e.getMessage}]")
      case e: IOException => Left(s"Can't load ${fileSource.descr}\n\n[${e.getMessage}]")
      case NonFatal(e) => Left(s"Exception trying to load ${fileSource.descr}\n\n[${e.getMessage}]")
    }
  }

  def chooseAndLoadFile(trimNewlines: Boolean): Option[(String, String)] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Load file", FileDialog.LOAD)
    dialog.setVisible(true)
    if (dialog.getFile == null) {
      None
    } else {
      val path = dialog.getDirectory + dialog.getFile

      loadFile(Source.fromFile(path, "UTF-8"), trimNewlines) match {
        case Right(fileContents) => Some(fileContents, path)
        case Left(errorMessage) => {
          Dialog.showMessage(null, errorMessage)
          None
        }
      }
    }
  }

  def saveFile(text: String, lastCharIsNewline: Boolean, path: String): Boolean = {
    val finalText = if (text.last == '\n') text else text + '\n'
    try {
      alwaysClose(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), "UTF-8"))) { writer =>
        writer.write(finalText)
        return true
      }
    } catch {
      case e: IOException => Dialog.showMessage(null, s"Can't save $path\n[${e.getMessage}]")
      case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to save $path\n[${e.getMessage}]")
    }
    false
  }

  def saveFileAs(text: String, lastCharIsNewline: Boolean): Option[String] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Save file", FileDialog.SAVE)
    dialog.setVisible(true)
    if (dialog.getFile != null) {
      val path = dialog.getDirectory + dialog.getFile
      if (saveFile(text, lastCharIsNewline, path))
        return Some(path)
    }
    None
  }

}
