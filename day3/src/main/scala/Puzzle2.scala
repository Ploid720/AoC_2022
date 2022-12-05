object Puzzle2
{
    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .grouped(3)
            .map(_ match {
                case List(a, b, c) => (a, b.toSet, c.toSet)
                case _ => throw new IllegalArgumentException
            })
            .map(t => t._1.find(e => t._2.contains(e) && t._3.contains(e)).get)
            .map(_ match {
                case c if c.isUpper => c - 'A' + 27
                case c => c - 'a' + 1
            })
            .sum)
    }
}