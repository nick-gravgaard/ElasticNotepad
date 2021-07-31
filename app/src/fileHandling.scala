import java.awt.FileDialog
import java.io.{BufferedWriter, FileNotFoundException, FileOutputStream, IOException, OutputStreamWriter}
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.reflect.Selectable.reflectiveSelectable
import scala.swing.Dialog
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

package object fileHandling {

  val appDirPath: Path = Paths.get(s"${System.getProperty("user.home")}/.elastic-notepad")
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
          e.printStackTrace()
      }
    }
  }

  def createAppDir(): Unit = {
    if (!Files.exists(appDirPath)) {
      Try(Files.createDirectory(appDirPath)) recoverWith {
        case exception => {
          Dialog.showMessage(null, exception.getMessage)
          Failure(exception)
        }
      }
    }
  }

  def loadScratchFile(): String = {
    createAppDir()

    Files.exists(scratchFilePath) match {
      case false => {
        Try(Files.createFile(scratchFilePath)) recoverWith {
          case exception => {
            Dialog.showMessage(null, exception.getMessage)
            Failure(exception)
          }
        }
        saveTextFile(assets.InitialText, scratchFilePath)
        assets.InitialText
      }
      case true => {
        loadTextFile(scratchFilePath) match {
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


  def loadTextFile(path: Path): Either[String, String] = {
    val fileSource = Source.fromFile(path.toString, "UTF-8")
    try {
      alwaysClose(fileSource) { handledFileSource =>
        val text = handledFileSource.getLines.mkString("\n")
        // we don't include the last newline in the buffer's text
        Right(if (text.lastOption == Some('\n')) text.dropRight(1) else text)
      }
    } catch {
      case e: FileNotFoundException => Left(s"Can't find ${fileSource.descr}\n\n[${e.getMessage}]")
      case e: IOException => Left(s"Can't load ${fileSource.descr}\n\n[${e.getMessage}]")
      case NonFatal(e) => Left(s"Exception trying to load ${fileSource.descr}\n\n[${e.getMessage}]")
    }
  }

  def chooseAndLoadTextFile: Option[(String, Path)] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Load file", FileDialog.LOAD)
    dialog.setVisible(true)
    if (dialog.getFile == null) {
      None
    } else {
      val path = Paths.get(dialog.getDirectory, dialog.getFile)

      loadTextFile(path) match {
        case Right(fileContents) => Some(fileContents, path)
        case Left(errorMessage) => {
          Dialog.showMessage(null, errorMessage)
          None
        }
      }
    }
  }

  def saveTextFile(text: String, path: Path): Boolean = {
    try {
      alwaysClose(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path.toString), "UTF-8"))) { writer =>
        // we always add a newline to the end of the saved file
        writer.write(text + '\n')
        return true
      }
    } catch {
      case e: IOException => Dialog.showMessage(null, s"Can't save $path\n[${e.getMessage}]")
      case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to save $path\n[${e.getMessage}]")
    }
    false
  }

  def saveTextFileAs(text: String): Option[Path] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Save file", FileDialog.SAVE)
    dialog.setVisible(true)
    if (dialog.getFile != null) {
      val path = Paths.get(dialog.getDirectory, dialog.getFile)
      if (saveTextFile(text, path))
        return Some(path)
    }
    None
  }

}
