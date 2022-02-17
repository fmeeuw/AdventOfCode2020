import Day1.parseInputLines

import scala.io.Source

object Day2 extends App with Helper {

  case class Policy(character: Char, range: Range)
  case class Input(policy: Policy, password: String)

  def parseInput(line: String): Input = {
    val Array(range, char, password) = line.split(' ')
    val Array(lower, upper) = range.split('-')
    Input(
      Policy(char.charAt(0), Range.inclusive(lower.toInt, upper.toInt)),
      password
    )
  }

  def part1 =
    println(
      parseInput().count(input =>
        matchesPolicyPart1(input.policy, input.password)
      )
    )

  def matchesPolicyPart1(policy: Policy, password: String): Boolean = {
    val occurences = password.count(_ == policy.character)
    policy.range.contains(occurences)
  }

  def parseInput(): Vector[Input] = {
    parseInputLines().map { parseInput }.toVector
  }

  def part2 =
    println(
      parseInput().count(input =>
        matchesPolicyPart2(input.policy, input.password)
      )
    )

  def matchesPolicyPart2(policy: Policy, password: String): Boolean = {
    val occursStart =
      password.charAt(policy.range.start - 1) == policy.character
    val occursEnd = password.charAt(policy.range.`end` - 1) == policy.character

    occursStart ^ occursEnd
  }

  part1
  part2
}
