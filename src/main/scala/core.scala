package object core {

  def maxConsecutive(list: List[Option[Int]]) : List[Option[Int]] = list match {
    // scala>     maxConsecutive(List(Some(1), Some(2), None, Some(4), None, None, Some(7), Some(8), Some(9)))
    // res1: List[Option[Int]] = List(Some(2), Some(2), None, Some(4), None, None, Some(9), Some(9), Some(9))
    case Nil => Nil
    case h::t => h match {
      case None => None :: maxConsecutive(list.drop(1))
      case Some(cell) => {
        val segment = list.takeWhile(_.isDefined).map(_.get)
        List.fill(segment.length)(Option(segment.max)) ::: maxConsecutive(list.drop(segment.length))
      }
    }
  }

  def calcMaxedWidthsPerLine(textWidthsPerLine: List[List[Int]]) : List[List[Int]] = {
    val maxNofCells = (for (textWidthsThisLine <- textWidthsPerLine) yield textWidthsThisLine.length).max

    val maxedWidthsPerColumn = for (c <- 0 until maxNofCells)
      yield maxConsecutive(for (textWidthsThisLine <- textWidthsPerLine)
        yield if (c < textWidthsThisLine.indices.last)
          Option(textWidthsThisLine(c)) else None)

    for (maxedWidthsThisLine <- maxedWidthsPerColumn.toList.transpose)
      yield maxedWidthsThisLine.takeWhile(_.isDefined).map(_.get)
  }

  def getCellsPerLine(textPerLine: List[String]): List[List[String]] = {
    for (textThisLine <- textPerLine) yield textThisLine.split('\t').toList
  }

  def measureWidthsPerLine(cellsPerLine: List[List[String]], measureText: String => Int): List[List[Int]] = {
    for (cellsThisLine <- cellsPerLine)
      yield for (textThisCell <- cellsThisLine)
        yield measureText(textThisCell)
  }

  def calcTabstopPositions(cellsPerLine: List[List[String]], measureText: String => Int): List[List[Int]] = {
    val cellWidthsPerLine = measureWidthsPerLine(cellsPerLine, measureText)

    for (maxedWidthsThisLine <- calcMaxedWidthsPerLine(cellWidthsPerLine))
      yield maxedWidthsThisLine.scanLeft(0)(_ + _).drop(1)
  }

  def toSpaces(text: String, nofIndentSpaces: Int): String = {
    val cellPaddingWidthSpaces = 2 // must be at least 2 so we can convert back to tabs
    val cellMinimumWidthSpaces = nofIndentSpaces - cellPaddingWidthSpaces
    val textPerLine = for (line <- text.split("\n")) yield line
    val cellsPerLine = getCellsPerLine(textPerLine.toList)
    def calcCellWidth(text: String): Int = math.max(text.length, cellMinimumWidthSpaces) + cellPaddingWidthSpaces
    val maxedWidthsPerLine = calcMaxedWidthsPerLine(measureWidthsPerLine(cellsPerLine, calcCellWidth))

    (for ((widthsThisLine, cellsThisLine) <- maxedWidthsPerLine.zip(cellsPerLine))
      yield (for ((cellText, width) <- cellsThisLine.zip(widthsThisLine :+ 0))
        yield cellText + (" " * (width - cellText.length))).mkString).mkString("\n")
  }

}
