val rules =
  """
    |47|53
    |97|13
    |97|61
    |97|47
    |75|29
    |61|13
    |75|53
    |29|13
    |97|29
    |53|29
    |61|53
    |97|53
    |61|29
    |47|13
    |75|47
    |97|75
    |47|61
    |75|61
    |47|29
    |75|13
    |53|13
    |""".stripMargin.trim.linesIterator
    .toList
    .filter(_.nonEmpty)
    .map(_.split("\\|"))
    .map { case Array(a, b) => a.toInt -> b.toInt }

val ascRules = rules
  .groupBy { case (a, _) => a }
  .map { case (a, list) => a -> list.map { case (_, b) => b }.toSet }

val descRules = rules.map { case (a, b) => b -> a }
  .groupBy { case (a, _) => a }
  .map { case (a, list) => a -> list.map { case (_, b) => b }.toSet }

val testCase =
  """
    |75,47,61,53,29
    |97,61,53,29,13
    |75,29,13
    |75,97,47,61,53
    |61,13,29
    |97,13,75,29,47
    |""".stripMargin.trim.linesIterator.toList.filter(_.nonEmpty).map(_.split(",").map(_.toInt))

def isOrderedPerRule(a: Int, b: Int): Boolean = {
  if(ascRules.contains(a) || descRules.contains(b)) {
    ascRules.get(a).forall(_.contains(b)) &&
      descRules.get(b).forall(_.contains(a))
  } else {
    true
  }
}

val part1 = testCase
  .filter(_.sliding(2, 1).forall { case Array(a, b) => isOrderedPerRule(a, b) })
  .map(e => e(e.size / 2))
  .sum

val part2 = testCase
  .filter(!_.sliding(2, 1).forall { case Array(a, b) => isOrderedPerRule(a, b) })
  .map { e =>
    (0 until (e.size - 1)).foreach { i =>
      (i + 1 until (e.size)).foreach { j =>
        if (!isOrderedPerRule(e(i), e(j))) {
          val tmp = e(i)
          e(i) = e(j)
          e(j) = tmp
        }
      }
    }
    
    e(e.size / 2)
  }.sum
