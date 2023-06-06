import scala.annotation.tailrec
import scala.collection.SortedSet

package object elasticTabstops:

  // convenience functions to wrap Java's unintuitive split method
  def splitByNewline(string: String) = string.split("\n", -1)  // -1 so we get trailing empty lines
  def splitByTabAndStrip(string: String) = string.split('\t')

  // Process runs of Some in list.
  // scala>    processAdjacent((l: List[Option[Int]]) => List.fill(l.length)(Some(l.flatten.max)),
  //                           List(Some(1), Some(2), None, Some(4), Some(5)))
  // res0: List[Option[Int]] = List(Some(2), Some(2), None, Some(5), Some(5))
  @tailrec
  private def processAdjacent[A](unprocessed: List[Option[A]], processed: List[Option[A]] = Nil)
                                (process: List[Option[A]] => List[Option[A]]): List[Option[A]] =
    unprocessed match
      case Nil => processed
      case head :: _ =>
        val (run, stillUnprocessed) = unprocessed.span(_.isDefined == head.isDefined)
        val newProcessed = if head.isDefined then process(run) else run
        processAdjacent(stillUnprocessed, processed ::: newProcessed)(process)

  // Replace each item in a run with its highest value.
  // scala>        maxAdjacent(List(Some(1), Some(2), None, Some(4), None, None, Some(7), Some(8), Some(9)))
  // res0: List[Option[Int]] = List(Some(2), Some(2), None, Some(4), None, None, Some(9), Some(9), Some(9))
  private def maxAdjacent(column: List[Option[Int]]): List[Option[Int]] =
    processAdjacent(column) { l =>
      List.fill(l.length)(Some(l.flatten.max))
    }

  private def calcMaxedWidthsPerLine(widthsPerLine: List[List[Int]]): List[List[Int]] =
    val maxNofCells = widthsPerLine.map(_.length).max
    val widthsPerCol = (0 until maxNofCells).map(idx => widthsPerLine.map(_.dropRight(1).lift(idx)))
    widthsPerCol.map(maxAdjacent).toList.transpose.map(_.takeWhile(_.isDefined).flatten)

  private def measureWidthsPerLine(cellsPerLine: List[List[String]], measureText: String => Int): List[List[Int]] =
    cellsPerLine.map(_.map(measureText(_)))

  def calcTabstopPositions(cellsPerLine: List[List[String]], measureText: String => Int): List[List[Int]] =
    val cellWidthsPerLine = measureWidthsPerLine(cellsPerLine, measureText)
    calcMaxedWidthsPerLine(cellWidthsPerLine).map(_.scanLeft(0)(_ + _).drop(1))

  def tabsToSpaces(text: String, nofIndentSpaces: Int): String =
    val cellPaddingWidthSpaces = 2  // must be at least 2 so we can convert back to tabs
    val cellMinimumWidthSpaces = nofIndentSpaces - cellPaddingWidthSpaces
    val cellsPerLine = splitByNewline(text).map(splitByTabAndStrip(_).toList).toList
    def calcCellWidth(text: String): Int = math.max(text.length, cellMinimumWidthSpaces) + cellPaddingWidthSpaces
    val maxedWidthsPerLine = calcMaxedWidthsPerLine(measureWidthsPerLine(cellsPerLine, calcCellWidth))

    maxedWidthsPerLine.zip(cellsPerLine).map { case (widthsThisLine, cellsThisLine) =>
      cellsThisLine.zip(widthsThisLine :+ 0).map { case (cellText, width) =>
        cellText + (" " * (width - cellText.length))
      }.mkString
    }.mkString("\n")

  // Replace each item in a run with None if all of its contents are empty strings.
  // scala>      nullifyEmptyRuns(List(Some(""), Some("a"), None, Some(""), Some("")))
  // res0: List[Option[String]] = List(Some(""), Some("a"), None, None,     None))
  private def nullifyEmptyRuns(column: Array[Option[String]]): Array[Option[String]] =
    processAdjacent(column.toList) { l =>
      if l.forall(_.contains("")) then List.fill[Option[String]](l.length)(None) else l
    }.toArray

  private def getMatchesPerLine(lines: Array[String]): Array[Map[Int, String]] =
    // a non-space followed by any number of chars that are either a non-space or a space followed by a non-space
    val cellTextRegEx = "[^ ](?:[^ ]| (?=[^ ]))*".r

    // get maps for each line containing the position of text and the text itself
    lines.map(cellTextRegEx.findAllMatchIn(_).map(m => m.start -> m.matched).toMap)

  private def getPossCellsFromText(lines: Array[String]): Array[Array[Option[String]]] =
    val matchesPerLine = getMatchesPerLine(lines)
    val positionsPerLine = matchesPerLine.map(_.keys)
    val maybeLastPosPerLine = positionsPerLine.map(_.toList.maxOption)

    // get sorted and unique possible cell positions using the flattened positions as varargs
    val allPositions = SortedSet(positionsPerLine.flatten*).toArray

    // create Options at every possible cell position
    allPositions.map { pos =>
      matchesPerLine.zip(maybeLastPosPerLine).map { case (matchesThisLine, maybeLastPosThisLine) =>
        if matchesThisLine.contains(pos) then Some(matchesThisLine(pos))
        else maybeLastPosThisLine.flatMap(lastPosThisLine => if pos <= lastPosThisLine then Some("") else None)
      }
    }

  def spacesToTabs(text: String): String =
    // split text into lines prepended with a non-whitespace character (so the first cell of each line is not empty)
    val lines = splitByNewline(text).map("|" + _)

    val possCellsPerCol = getPossCellsFromText(lines)

    // replace empty columns with Nones, transpose, remove Nones, and join with tabs
    val textPerLine = possCellsPerCol.toList.map(nullifyEmptyRuns).transpose.map(_.flatten).map(_.mkString("\t"))

    // finally, drop previously prepended non-whitespace character from each line and join with newlines
    textPerLine.map(_.drop(1)).mkString("\n")
