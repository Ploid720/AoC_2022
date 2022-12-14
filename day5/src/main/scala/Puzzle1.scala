import scala.annotation.tailrec

object Puzzle1
{
    def main(args: Array[String]): Unit =
    {
        @tailrec
        def moveCrates(stacks: Map[Int, List[Char]],
            stepsLeft: Int, from: Int, to: Int): 
            Map[Int, List[Char]] =
        {
            if (stepsLeft < 1)
                stacks
            else
            {
                val el = stacks(from).head
                moveCrates(stacks
                    + (from -> stacks(from).tail)
                    + (to -> (el :: stacks(to))),
                    stepsLeft - 1, from, to)
            }
        }

        val input = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
        
        val stackLines = input.takeWhile(s => !s.trim.isEmpty)
        val stack = stackLines
            .map(_.toList
                .zipWithIndex
                filter(_._1.isLetter))
            .zipWithIndex
            .flatMap(t => t._1.map(e => (e._1, e._2, t._2)))
            .groupBy(_._2)
            .toList
            .map(_._2)
            .sortBy(_.head._2)
            .map(_.sortBy(_._3))
            .map(_.map(_._1))
            .zipWithIndex
            .map(_.swap)
            .toMap

        println(input.drop(stackLines.size)
            .filter(s => !s.trim.isEmpty)
            .map(s => "[^\\d]+".r
                .split(s)
                .toList
                .tail)
            .map(_ match {
                case List(a, b, c) => (a.toInt, b.toInt - 1, c.toInt - 1)
                case _ => throw new IllegalArgumentException
            })
            .foldLeft(stack)((st, m) => moveCrates(st, m._1, m._2, m._3))
            .toList
            .sortBy(_._1)
            .map(_._2.head)
            .mkString)
    }
}