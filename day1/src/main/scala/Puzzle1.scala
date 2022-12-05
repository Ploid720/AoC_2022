object Puzzle1
{
    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .foldLeft(List[Int]())((acc, el) => (acc, el) match {
                case (l, s) if s.trim.isEmpty => 0 :: l
                case (h :: t, s) => (h + s.toInt) :: t
                case (_, s) => List(s.toInt)
            })
            .max)
    }
}