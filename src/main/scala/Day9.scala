object Day9 extends App with Helper {

  def parseInput: Vector[Long] =
    parseInputLines().map(_.toLong).toVector

  def part1 = {
    val input = parseInput
    input.sliding(25 + 1).foreach { seq =>
      val (preamble, number) = seq.splitAt(25)
      val valid = isValidNumber(preamble, number.head)
      println(s"$number is valid = $valid")
    }
  }

  def isValidNumber(preamble: Vector[Long], number: Long): Boolean = {
    preamble.combinations(2).exists(_.sum == number)
  }

  def part2 = {
    val input = parseInput
    val numberToFind: Long = 466456641L

    val solutions = for {
      i <- 0 to input.size
      size <- 2 until 20
      sum = input.slice(i, i + size).sum
      if sum == numberToFind
    } yield input.slice(i, i + size)

    val solution = solutions.head
    println(solution)
    println(solution.min + solution.max)
  }

  part1
  part2
}
