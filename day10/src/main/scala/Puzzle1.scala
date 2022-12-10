import scala.annotation.tailrec

object Puzzle1
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
            .foldLeft((1, 0, List[Int]()))((z, instr) => {
                val x = z._1 + instr._1
                val cy = z._2 + instr._2
                val lo = (z._2 + 20) % 40
                val hi = (z._2 + instr._2 + 20) % 40
                if ((lo > hi) && (cy <= 220))
                    (x, cy, (z._1 * (cy - hi)) :: z._3)
                else
                    (x, cy, z._3)
            })
            ._3
            .sum)
    }
}