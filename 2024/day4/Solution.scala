import scala.collection.Seq

def timed(fn: => Unit): Unit = {
  val start = System.currentTimeMillis()
  fn
  val end = System.currentTimeMillis()

  println(s"Runtime - ${end - start}ms")
}

def equalsStringOrReverse(left: String, right: String) = left == right || left == right.reverse

def matchHorizontal(
 input: Array[Array[Char]],
 searchWord: String,
 i: Int = 0,
 j: Int = 0
)(matcher: (String, String) => Boolean) = {
  if((j + searchWord.size - 1) < input.head.size) {
    val horizontal = (0 until searchWord.size).map { offset => input(i)(j + offset) }.mkString
    if (matcher(horizontal, searchWord)) 1 else 0
  } else {
    0
  }
}

def matchVertical(
 input: Array[Array[Char]],
 searchWord: String,
 i: Int = 0,
 j: Int = 0
)(matcher: (String, String) => Boolean) = {
  if(i + searchWord.size - 1 < input.size) {
    val vertical = (0 until searchWord.size).map { offset => input(i + offset)(j) }.mkString
    if (matcher(vertical, searchWord)) 1 else 0
  } else {
    0
  }
}

def matchLeftDiagonal(
 input: Array[Array[Char]],
 searchWord: String,
 i: Int = 0,
 j: Int = 0
)(matcher: (String, String) => Boolean) = {
  if(i + searchWord.size - 1 < input.size && j + searchWord.size - 1 < input.head.size) {
    val diagonal = (0 until searchWord.size).map { offset => input(i + offset)(j + offset) }.mkString
    if (matcher(diagonal, searchWord)) 1 else 0
  } else {
    0
  }
}

def matchRightDiagonal(
  input: Array[Array[Char]],
  searchWord: String,
  i: Int = 0,
  j: Int = 0
)(matcher: (String, String) => Boolean) = {
  if(i + searchWord.size - 1 < input.size && j + searchWord.size - 1 < input.head.size) {
    val reverseDiagonal = (0 until searchWord.size).map { offset => input(i + (searchWord.size - 1 - offset))(j + offset) }.mkString
    if (matcher(reverseDiagonal, searchWord)) 1 else 0
  } else {
    0
  }
}

def matchCrossingDiagonal(
 input: Array[Array[Char]],
 searchWord: String,
 i: Int = 0,
 j: Int = 0
)(matcher: (String, String) => Boolean) = {
  if(i + searchWord.size - 1 < input.size && j + searchWord.size - 1 < input.head.size) {
    val diagonal = (0 until searchWord.size).map { offset => input(i + offset)(j + offset) }.mkString
    val reverseDiagonal = (0 until searchWord.size).map { offset => input(i + (searchWord.size - 1 - offset))(j + offset) }.mkString

    if (matcher(diagonal, searchWord) && matcher(reverseDiagonal, searchWord)) 1 else 0
  } else {
    0
  }
}

def countMatches(
  input: Array[Array[Char]],
  searchWord: String,
  matcher: (String, String) => Boolean,
  strategies: Seq[(Array[Array[Char]], String, Int, Int) => ((String, String) => Boolean) => Int],
  i: Int = 0,
  j: Int = 0,
  visited: collection.mutable.Set[(Int, Int)] = collection.mutable.Set()
): Int = {
  if (i >= input.size || j >= input.head.size || visited.contains(i -> j)) return 0

  visited.add(i -> j)

  strategies.map(_(input, searchWord, i, j)(matcher)).sum +
    countMatches(input, searchWord, matcher, strategies, i + 1, j, visited) +
    countMatches(input, searchWord, matcher, strategies, i, j + 1,  visited)
}

val testCasePart1 =
  """MMMSXXMASM
    |MSAMXMSMSA
    |AMXSXMAAMM
    |MSAMASMSMX
    |XMASAMXAMM
    |XXAMMXXAMA
    |SMSMSASXSS
    |SAXAMASAAA
    |MAMMMXMMMM
    |MXMXAXMASX
    |""".stripMargin

timed {
  val part1 = countMatches(
    input = testCasePart1.linesIterator.toArray.filter(_.nonEmpty).map(_.toArray),
    searchWord = "XMAS",
    matcher = equalsStringOrReverse,
    strategies = Seq(
      matchHorizontal,
      matchVertical,
      matchLeftDiagonal,
      matchRightDiagonal,
    )
  )
  println(part1)
}

val testCasePart2 =
  """.M.S......
    |..A..MSMS.
    |.M.S.MAA..
    |..A.ASMSM.
    |.M.S.M....
    |..........
    |S.S.S.S.S.
    |.A.A.A.A..
    |M.M.M.M.M.
    |..........""".stripMargin

timed {
  val part2 = countMatches(
    input = testCasePart2.linesIterator.toArray.filter(_.nonEmpty).map(_.toArray),
    searchWord = "MAS",
    matcher = equalsStringOrReverse,
    strategies = Seq(
      matchCrossingDiagonal
    )
  )
  println(part2)
}

