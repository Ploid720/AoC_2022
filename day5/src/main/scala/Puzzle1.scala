import scala.annotation.tailrec

object Puzzle1
{
    def main(args: Array[String]): Unit =
    {
        @tailrec
        def moveCrates(stack: Array[List[Char]], stepsLeft: Int, from: Int, to: Int): 
            Array[List[Char]] =
        {
            if (stepsLeft < 1)
                stack
            else
            {
                val el = stack(from).head
                stack(from) = stack(from).tail
                stack(to) = el :: stack(to)
                moveCrates(stack, stepsLeft - 1, from, to)
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
            .toArray
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
            .map(_.head)
            .mkString)
    }
}