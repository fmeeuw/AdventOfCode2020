import scala.annotation.tailrec

object Day10 extends App with Helper {

  def parseInput: Vector[Int] =
    parseInputLines().map(_.toInt).toVector

  def part1 = {
    val input = parseInput.sorted
    val sortedAdapters = 0 +: input :+ (input.max + 3)
    println(sortedAdapters)
    val diffMap = sortedAdapters
      .sliding(2)
      .map { case Vector(a, b) => b - a }
      .toVector
      .groupBy(identity)

    println(
      diffMap.getOrElse(1, Vector.empty).size * diffMap
        .getOrElse(3, Vector.empty)
        .size
    )
  }

  def part2 = {
    val inputAdapters = parseInput.sorted.toList
    val combos =
      combinations(Map(0 -> 1L).withDefaultValue(0), inputAdapters.max + 3)(
        (inputAdapters) :+ inputAdapters.max + 3
      )
    println(combos)
  }

  def combinations(cache: Map[Int, Long], lastToMatch: Int)(
      jolts: List[Int]
  ): Long =
    jolts match {
      case Nil => cache(lastToMatch)
      case jolt :: newJolts =>
        val arrangements = cache(jolt - 3) + cache(jolt - 2) + cache(jolt - 1)
        val newCache = cache + (jolt -> arrangements)
        combinations(newCache, lastToMatch)(newJolts)
    }

  part1
  part2
}
