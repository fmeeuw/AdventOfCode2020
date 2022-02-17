import Day1.parseInputLines
import Day2.{Input, parseInput}

import scala.io.Source

object Day3 extends App with Helper {

  case class World(base: Vector[Vector[Boolean]]) {
    def isOpen(x: Int, y: Int) = {
      base(y)(x % base(y).length)
    }
    def height = base.size

  }

  def parseInput: Vector[Vector[Boolean]] = {
    parseInputLines()
      .map(_.map(_ == '.').toVector)
      .toVector
  }

  def part1 = {
    val world: World = World(parseInput)
    val trees = countTreesRec(world, 0, 0, 3, 1)
    println(trees)
  }

  def countTreesRec(
      world: World,
      x: Int,
      y: Int,
      deltaX: Int,
      deltaY: Int
  ): Int = {
    if (y >= world.height) {
      0
    } else {
      countTreesRec(world, x + deltaX, y + deltaY, deltaX, deltaY) + (if (
                                                                        world.isOpen(
                                                                          x,
                                                                          y
                                                                        )
                                                                      ) 0
                                                                      else 1)
    }
  }

  def part2() = {
    val world: World = World(parseInput)
    val result = 1L *
      countTreesRec(world, 0, 0, 1, 1) *
      countTreesRec(world, 0, 0, 3, 1) *
      countTreesRec(world, 0, 0, 5, 1) *
      countTreesRec(world, 0, 0, 7, 1) *
      countTreesRec(world, 0, 0, 1, 2)

    println(result)
  }

  part1
  part2
}
