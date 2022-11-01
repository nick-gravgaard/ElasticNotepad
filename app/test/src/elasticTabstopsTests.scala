import org.scalatest.flatspec.AnyFlatSpec

import elasticTabstops.{calcTabstopPositions, spacesToTabs, tabsToSpaces}


class ElasticTabstopsSpec extends AnyFlatSpec {

  "tabsToSpaces" should "replace tabs with spaces correctly" in {
    assert(tabsToSpaces("\ty", 4) == "    y")
    assert(tabsToSpaces("x\ty", 4) == "x   y")
    assert(tabsToSpaces("xxxxxxx\ty", 4) == "xxxxxxx  y")

    val given1 = List(
      "\ty",
      "xxxxxxx\ty"
    ).mkString("\n")

    val expected1 = List(
      "         y",
      "xxxxxxx  y"
    ).mkString("\n")

    assert(tabsToSpaces(given1, 4) == expected1)

    val given2 = List(
      "\t",
      "xxxxxxx"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxxxxxx"
    ).mkString("\n")

    assert(tabsToSpaces(given2, 4) == expected2)
  }

  "spacesToTabs" should "replace spaces with tabs correctly" in {
    assert(spacesToTabs("    y") == "\ty")
    assert(spacesToTabs("x   y") == "x\ty")
    assert(spacesToTabs("xxxxxxx  y") == "xxxxxxx\ty")

    val given1 = List(
      "         y",
      "xxxxxxx  y"
    ).mkString("\n")

    val expected1 = List(
      "\ty",
      "xxxxxxx\ty"
    ).mkString("\n")

    assert(spacesToTabs(given1) == expected1)

    val given2 = List(
      "       ",
      "xxxxxxx"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxxxxxx"
    ).mkString("\n")

    assert(spacesToTabs(given2) == expected2)
  }

}
