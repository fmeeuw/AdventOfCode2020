import scala.io.Source

trait Helper {
  def parseInputLines(
      day: String = getClass.getSimpleName.filterNot(_ == '$'),
      additional: String = ""
  ): Iterator[String] =
    Source.fromResource(s"$day$additional.txt").getLines()
}
