import scala.io.Source

object Day1 extends App with Helper {

  def parseInput(): Vector[Int] = {
    parseInputLines().map(_.toInt).toVector
  }

  val numbers = parseInput()

  def part1 = {
    val pairs = for {
      x <- numbers
      y <- numbers.filter(y => y != x && y + x == 2020)
    } yield (x, y)

    println(pairs.map { case (x, y) => x * y })
  }

  def part2 = {
    val pairs = for {
      x <- numbers
      y <- numbers.filter(y => y != x && y + x <= 2020)
      z <- numbers.filter(z => z != x && z != y && z + y + x == 2020)
    } yield (x, y, z)
    println(pairs.map { case (x, y, z) => x * y * z })
  }

  part1
  part2
}
