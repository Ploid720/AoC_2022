object Puzzle2
{
    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .flatMap(s => s.toList)
            .sliding(14)
            .zipWithIndex
            .find(t => t._1.distinct.length == t._1.length)
            .map(t => t._2 + 14)
            .getOrElse("Not found"))
    }
}