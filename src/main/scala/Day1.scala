import scala.io.Source

object Day1 extends App {

  def parseInput(): Vector[String] = {
    Source.fromResource("Day1Input.txt").getLines().toVector
  }

  parseInput().foreach(println)
}
