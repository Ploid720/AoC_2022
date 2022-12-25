import scala.annotation.tailrec
import collection.immutable.Queue

object Puzzle
{
    def toSnafu(n: Long): String =
    {
        val digitMap = Map(
            -2 -> '=',
            -1 -> '-',
            0 -> '0',
            1 -> '1',
            2 -> '2'
        ).map(t => (t._1.toLong, t._2))

        @tailrec
        def buildSnafu(left: Long, parsed: List[Char]): String =
        {
            if (left == 0)
                parsed.mkString
            else
            {
                val baseDigit = left % 5
                val baseCarry = left / 5
                val dn = if (baseDigit > 2) 
                    (baseDigit - 5, baseCarry + 1) else (baseDigit, baseCarry)
                buildSnafu(dn._2, digitMap(dn._1) :: parsed)
            }
        }
        
        buildSnafu(n, List())
    }

    def main(args: Array[String]): Unit =
    {
        println(toSnafu(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .filter(_.nonEmpty)
            .map(_.toList
                .foldLeft(0l)((z, a) => (z * 5) + (a match {
                    case '=' => -2
                    case '-' => -1
                    case '0' => 0
                    case '1' => 1
                    case '2' => 2
                    case _ => throw new IllegalArgumentException
                })))
            .sum))
    }
}