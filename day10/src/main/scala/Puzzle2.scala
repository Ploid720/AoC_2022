import scala.annotation.tailrec

object Puzzle2
{
    def main(args: Array[String]): Unit =
    {
        val addxPat = "addx (-?\\d+)".r

        println(io.Source
            .fromResource("input.txt")
            .getLines
            .map(_ match {
                case "noop" => (0, 1)
                case addxPat(x) => (x.toInt, 2)
            })
            .foldLeft((1, 0, List[Char]()))((z, instr) => 
                (z._1 + instr._1, z._2 + instr._2, (z._2 until (z._2 + instr._2))
                    .toList
                    .map(_ % 40)
                    .map(n => (if (n >= (z._1 - 1) && n <= (z._1 + 1)) '#' else '.', n))
                    .flatMap(t => if (t._2 == 39) List(t._1, '\n') else List(t._1))
                    .reverse
                    ::: z._3))
            ._3
            .reverse
            .mkString)
    }
}