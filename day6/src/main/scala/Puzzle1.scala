object Puzzle1
{
    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .flatMap(s => s.toList)
            .sliding(4)
            .zipWithIndex
            .find(t => t._1.distinct.length == t._1.length)
            .map(t => t._2 + 4)
            .getOrElse("Not found"))
    }
}