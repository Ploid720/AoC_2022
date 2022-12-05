object Puzzle1
{
    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(s => (s, s.length / 2))
            .map(t => (t._1.take(t._2), t._1.drop(t._2).toSet))
            .map(t => t._1.find(e => t._2.contains(e)).get)
            .map(_ match {
                case c if c.isUpper => c - 'A' + 27
                case c => c - 'a' + 1
            })
            .sum)
    }
}