val testCase =
  """
    |7 6 4 2 1
    |1 2 7 8 9
    |9 7 6 2 1
    |1 3 2 4 5
    |8 6 4 4 1
    |1 3 6 7 9
    |""".stripMargin
    .linesIterator
    .filter(_.nonEmpty)
    .toList
    .map(_.split("\\s").map(_.toInt).toList)

def checkList(list: List[Int]) = {
  val diffs = list.sliding(2, 1).map { case List(a, b) => b - a }.toList
  (diffs.min > 0 && diffs.max <= 3) || (diffs.min >= -3 && diffs.max < 0)
}

val part1 = testCase.count { list => checkList(list) }

val part2 = testCase.count { list =>
  checkList(list) ||
    list.zipWithIndex.exists { case (_, i) => checkList(list.take(i) ++ list.takeRight(list.size - (i + 1))) }
}
