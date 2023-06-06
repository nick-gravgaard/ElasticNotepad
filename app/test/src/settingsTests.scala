import org.scalatest.flatspec.AnyFlatSpec
import settings.Settings.{addMissingSettingsToText, textToMaps}


class SettingsSpec extends AnyFlatSpec {
  "textToMaps" should "create maps correctly" in {
    val given1 = List(
      "    # some comment",
      "    | some other comment",
      "    active setting    :    some value 2    ",
      "    >    inactive setting    :    some value 1    ",
    ).map(_ + '\n').mkString

    val expected1 = (
      Map("active setting" -> "some value 2"),
      Map("inactive setting" -> "some value 1")
    )

    assert(textToMaps(given1) == expected1)
  }

  "addMissingSettingsToText" should "work correctly" in {
    val given1 = List(
      "# Default settings (delete leading '>' to override)",
      "Elastic font:\t\"Merriweather\", 19\t| Used when elastic tabstops is on (can be proportional)",
      "Non-elastic font:\t\"Inconsolata\", 23\t| Used when elastic tabstops is off (monospaced is best)",
      ">Non-elastic tab size:\t4\t| The indent size in non-elastic files",
      ">Files on disk are non-elastic:\ttrue\t| Convert to elastic tabstops on load, save as non-elastic",
      ">Theme:\tDark\t| \"Light\" or \"Dark\". Restart to take effect",
      ">Scale:\t1.25\t| Multiplier to scale UI elements by. Restart to take effect",
    ).map(_ + '\n').mkString

    val expected1 = List(
      "# Default settings (delete leading '>' to override)",
      "Elastic font:\t\"Merriweather\", 19\t| Used when elastic tabstops is on (can be proportional)",
      "Non-elastic font:\t\"Inconsolata\", 23\t| Used when elastic tabstops is off (monospaced is best)",
      ">Non-elastic tab size:\t4\t| The indent size in non-elastic files",
      ">Files on disk are non-elastic:\ttrue\t| Convert to elastic tabstops on load, save as non-elastic",
      ">Theme:\tDark\t| \"Light\" or \"Dark\". Restart to take effect",
      ">Scale:\t1.25\t| Multiplier to scale UI elements by. Restart to take effect",
      "",
      "# The following settings are missing (delete leading '>' to override)",
      ">Empty column width:\t1.8\t| Measured in multiples of line height (ems)",
      ">Column padding:\t0.625\t| Measured in multiples of line height (ems)",
    ).map(_ + '\n').mkString

    assert(addMissingSettingsToText(given1) == expected1)
  }
}
