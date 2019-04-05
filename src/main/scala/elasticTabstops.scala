import scala.collection.SortedSet


package object elasticTabstops {

  // convenience functions to wrap Java's unintuitive split method
  def split(string: String, char: Char) = string.split(char.toString, -1)  // -1 so we get trailing empty strings
  def splitAndStrip(string: String, char: Char) = string.split(char)

  // Group runs of adjacent Somes and Nones.
  // scala>            groupAdjacent(List(     Some(1), Some(2),       None,       Some(4),       None, None,       Some(7), Some(8), Some(9)))
  // res1: List[List[Option[Int]]] = List(List(Some(1), Some(2)), List(None), List(Some(4)), List(None, None), List(Some(7), Some(8), Some(9)))
  @annotation.tailrec
  def groupAdjacent[A](remaining: List[Option[A]], processed: List[List[Option[A]]] = Nil): List[List[Option[A]]] =
    remaining.headOption match {
      case None => processed
      case Some(firstRemaining) => {
        val segment = firstRemaining match {
          case None => remaining.takeWhile(_.isEmpty)
          case Some(_) => remaining.takeWhile(_.isDefined)
        }
        groupAdjacent(remaining.drop(segment.length), processed ::: List(segment))
      }
    }

  // Segments are runs of Some. Replace each item in a segment with the highest value in that segment.
  // scala>     maxConsecutive(List(Some(1), Some(2), None, Some(4), None, None, Some(7), Some(8), Some(9)))
  // res1: List[Option[Int]] = List(Some(2), Some(2), None, Some(4), None, None, Some(9), Some(9), Some(9))
  def maxConsecutive(list: List[Option[Int]]): List[Option[Int]] =
    groupAdjacent(list).flatMap { segment =>
      segment.headOption.getOrElse(None) match {
        case None => segment
        case Some(_) => List.fill(segment.length)(Some(segment.flatten.max))
      }
    }

  def calcMaxedWidthsPerLine(textWidthsPerLine: List[List[Int]]) : List[List[Int]] = {
    val maxNofCells = textWidthsPerLine.map(_.length).max

    val maxedWidthsPerColumn = (0 until maxNofCells).map(c =>
      maxConsecutive(textWidthsPerLine.map(_.dropRight(1).lift(c))))

    maxedWidthsPerColumn.toList.transpose.map(_.takeWhile(_.isDefined).flatten)
  }

  def measureWidthsPerLine(cellsPerLine: List[List[String]], measureText: String => Int): List[List[Int]] =
    cellsPerLine.map(_.map(measureText(_)))

  def calcTabstopPositions(cellsPerLine: List[List[String]], measureText: String => Int): List[List[Int]] = {
    val cellWidthsPerLine = measureWidthsPerLine(cellsPerLine, measureText)

    calcMaxedWidthsPerLine(cellWidthsPerLine).map(_.scanLeft(0)(_ + _).drop(1))
  }

  def tabsToSpaces(text: String, nofIndentSpaces: Int): String = {
    val cellPaddingWidthSpaces = 2  // must be at least 2 so we can convert back to tabs
    val cellMinimumWidthSpaces = nofIndentSpaces - cellPaddingWidthSpaces
    val cellsPerLine = split(text, '\n').map(splitAndStrip(_, '\t').toList).toList
    def calcCellWidth(text: String): Int = math.max(text.length, cellMinimumWidthSpaces) + cellPaddingWidthSpaces
    val maxedWidthsPerLine = calcMaxedWidthsPerLine(measureWidthsPerLine(cellsPerLine, calcCellWidth))

    maxedWidthsPerLine.zip(cellsPerLine).map { case (widthsThisLine, cellsThisLine) =>
      cellsThisLine.zip(widthsThisLine :+ 0).map { case (cellText, width) =>
        cellText + (" " * (width - cellText.length))
      }.mkString
    }.mkString("\n")
  }

  // Segments are runs of Some. Replace each item in a segment with None if none of them contain any text.
  // scala>      replaceEmptyRuns(List(Some(""), Some("a"), None, Some(""), Some("")))
  // res1: List[Option[String]] = List(Some(""), Some("a"), None, None,     None))
  def replaceEmptyRuns(list: List[Option[String]]): List[Option[String]] =
    groupAdjacent(list).flatMap { segment =>
      segment.headOption.getOrElse(None) match {
        case None => segment
        case Some(_) =>
          if (segment.forall(_.contains("")))
            List.fill[Option[String]](segment.length)(None)
          else
            segment
      }
    }

  def spacesToTabs(text: String): String = {
    // a non-space followed by any number of chars that are either a non-space or a space followed by a non-space
    val cellTextRegEx = "[^ ](?:[^ ]| (?=[^ ]))*".r

    // prefix each line with a non-whitespace character (as we want to treat the start of each line as a column of text)
    val prefixedTextPerLine = split(text, '\n').map("|" + _)

    // get maps for each line containing the position of text and the text itself
    val matchesPerLine = prefixedTextPerLine.map(cellTextRegEx.findAllMatchIn(_).map(m => m.start -> m.matched).toMap)

    val allPositions = SortedSet(matchesPerLine.map(_.keys).toList.flatten: _*)

    // for each line, create matched or empty strings at every possible cell position
    val possCellsPerLine = matchesPerLine.map(matchesThisLine =>
      allPositions.toArray.map(position =>
        if (matchesThisLine.contains(position)) Some(matchesThisLine(position)) else Some("")))

    // we know that empty strings at the end of the line cannot be cells, so replace them with None
    val possCellsPerLine2 = possCellsPerLine.map { possCellsThisLine =>
      val nofTrailingEmpties = possCellsThisLine.reverse.takeWhile(_.contains("")).length
      possCellsThisLine.take(possCellsThisLine.length - nofTrailingEmpties) ++ List.fill(nofTrailingEmpties)(None)
    }

    // transpose, replace empty columns with Nones, transpose back, remove Nones, and join with tabs
    val textPerLine = possCellsPerLine2.toList.transpose.map(replaceEmptyRuns).transpose.map(_.flatten).map(_.mkString("\t"))

    // finally, drop previously inserted non-whitespace character from each line and join with newlines
    textPerLine.map(_.drop(1)).mkString("\n")
  }
}
