import org.scalatest.FlatSpec

import elasticTabstops.{calcTabstopPositions, spacesToTabs, tabsToSpaces}


class ElasticTabstopsSpec extends FlatSpec {

  "Tabs" should "be replaced by spaces correctly" in {
    assert(tabsToSpaces("\ty", 4) == "    y")
    assert(tabsToSpaces("x\ty", 4) == "x   y")
    assert(tabsToSpaces("xxxxxxx\ty", 4) == "xxxxxxx  y")
    assert(tabsToSpaces(List(
      "\ty",
      "xxxxxxx\ty"
    ).mkString("\n"), 4) == List(
      "         y",
      "xxxxxxx  y"
    ).mkString("\n")
    )
    assert(tabsToSpaces(List(
      "\t",
      "xxxxxxx"
    ).mkString("\n"), 4) == List(
      "",
      "xxxxxxx"
    ).mkString("\n")
    )
  }

  "Spaces" should "be replaced by tabs correctly" in {
    assert(spacesToTabs("    y") == "\ty")
    assert(spacesToTabs("x   y") == "x\ty")
    assert(spacesToTabs("xxxxxxx  y") == "xxxxxxx\ty")
    assert(spacesToTabs(List(
      "         y",
      "xxxxxxx  y"
    ).mkString("\n")) == List(
      "\ty",
      "xxxxxxx\ty"
    ).mkString("\n")
    )
    assert(spacesToTabs(List(
      "       ",
      "xxxxxxx"
    ).mkString("\n")) == List(
      "",
      "xxxxxxx"
    ).mkString("\n")
    )
  }

}
