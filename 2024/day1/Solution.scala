val (left, right) =
  """
    |3   4
    |4   3
    |2   5
    |1   3
    |3   9
    |3   3
    |""".stripMargin
    .linesIterator
    .toList
    .filter(_.nonEmpty)
    .map(_.split("\\s+").toList)
    .map { case List(a, b) => a.toInt -> b.toInt }
    .unzip

val part1 = left.sorted.zip(right.sorted).map { case (a, b) => scala.math.abs(a - b) }.sum

val counts = right.map(_ -> 1)
  .groupBy { case (value, _) => value }
  .map { case (value, count) => value -> count.map(_._2).sum }

val part2 = left.map { case (value) => value * counts.get(value).getOrElse(0) }.sum
