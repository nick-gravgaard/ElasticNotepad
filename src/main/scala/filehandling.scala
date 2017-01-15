import java.awt.FileDialog
import java.io.{BufferedWriter, FileNotFoundException, FileOutputStream, IOException, OutputStreamWriter}
import scala.io.Source
import scala.swing.Dialog
import scala.util.control.NonFatal

package object filehandling {

  def loadFile(): Option[(String, String)] = {
    val dialog = new FileDialog(null.asInstanceOf[FileDialog], "Load file", FileDialog.LOAD)
    dialog.setVisible(true)
    if (dialog.getFile == null) {
      None
    } else {
      val path = dialog.getDirectory + dialog.getFile

      try {
        val fileSource = Source.fromFile(path, "UTF-8")
        try {
          return Some((fileSource.getLines.mkString("\n"), path))
        } catch {
          case e: IOException => Dialog.showMessage(null, s"Can't load $path\n[${e.getMessage}]")
          case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to load $path\n[${e.getMessage}]")
        } finally {
          fileSource.close
        }
      } catch {
        case e: FileNotFoundException => Dialog.showMessage(null, s"Can't find $path\n[${e.getMessage}]")
        case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to find $path\n[${e.getMessage}]")
      }
      None

    }
  }

  def saveFile(text: String, path: String): Boolean = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(path), "UTF-8"))
    try {
      writer.write(text)
      return true
    } catch {
      case e: IOException => Dialog.showMessage(null, s"Can't save $path\n[${e.getMessage}]")
      case NonFatal(e) => Dialog.showMessage(null, s"Exception trying to save $path\n[${e.getMessage}]")
    } finally {
      writer.close
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
