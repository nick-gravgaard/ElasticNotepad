import java.awt.FileDialog
import java.io.{BufferedWriter, FileNotFoundException, FileOutputStream, IOException, OutputStreamWriter}
import scala.io.Source
import scala.swing.Dialog
import scala.util.control.NonFatal

package object filehandling {

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

  def loadFile(): Option[(String, String)] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Load file", FileDialog.LOAD)
    dialog.setVisible(true)
    if (dialog.getFile == null) {
      None
    } else {
      val path = dialog.getDirectory + dialog.getFile

      try {
        alwaysClose(Source.fromFile(path, "UTF-8")) { fileSource =>
          return Some((fileSource.getLines.mkString("\n"), path))
        }
      } catch {
        case e: FileNotFoundException => Dialog.showMessage(null, s"Can't find $path\n\n[${e.getMessage}]")
        case e: IOException => Dialog.showMessage(null, s"Can't load $path\n\n[${e.getMessage}]")
        case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to load $path\n\n[${e.getMessage}]")
      }
      None

    }
  }

  def saveFile(text: String, path: String): Boolean = {
    try {
      alwaysClose(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), "UTF-8"))) { writer =>
        writer.write(text)
        return true
      }
    } catch {
      case e: IOException => Dialog.showMessage(null, s"Can't save $path\n[${e.getMessage}]")
      case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to save $path\n[${e.getMessage}]")
    }
    false
  }

  def saveFileAs(text: String): Option[String] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Save file", FileDialog.SAVE)
    dialog.setVisible(true)
    if (dialog.getFile != null) {
      val path = dialog.getDirectory + dialog.getFile
      if (saveFile(text, path))
        return Some(path)
    }
    None
  }

}
