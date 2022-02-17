import Day1.parseInputLines

import scala.io.Source

object Day6 extends App with Helper {

  def parseInput: Vector[Vector[String]] = {
    parseInputLines()
      .foldLeft(Vector(Vector.empty[String])) { (agg, line) =>
        if (line.isEmpty) Vector.empty +: agg
        else (agg.head :+ line) +: agg.tail
      }
      .toVector
  }

  def part1 = {
    val groups = parseInput
    val answers: Seq[Set[Char]] = groups.map { group =>
      val groupSet = group.map(person => person.toSet)
      groupSet.foldLeft(Set.empty[Char])(_.union(_))
    }

    val count = answers.map(_.size).sum
    println(count)
  }

  def part2 = {
    val groups = parseInput
    val answers: Seq[Set[Char]] = groups.map { group =>
      val groupSet = group.map(person => person.toSet)
      groupSet.foldLeft("abcdefghijklmnopqrstuvwxyz".toSet)(_.intersect(_))
    }

    val count = answers.map(_.size).sum
    println(count)
  }

  part1
  part2
}
