import scala.annotation.tailrec

object Puzzle1
{
    case class Move(predicate: (((Int, Int)) => Boolean) => Boolean, 
        direction: (Int, Int))

    def spreadElves(elves: Set[(Int, Int)], 
        moves: List[Move],
        roundLimit: Int): Set[(Int, Int)] =
    {
        val nbOffs = List(
            (-1, -1), (0, -1), (1, -1),
            (-1,  0),          (1,  0),
            (-1,  1), (0,  1), (1,  1)
        )
        def neighbourhood(pos: (Int, Int)): List[(Int, Int)] = 
            nbOffs.map(p => (pos._1 + p._1, pos._2 + p._2))

        def findMove(left: List[Move], validator: 
            ((Int, Int)) => Boolean, parsed: List[Move]): Option[Move] =
        {
            left match {
                case h :: t => {
                    if (h.predicate(validator))
                        Some(h)
                    else
                        findMove(t, validator, h :: parsed)
                }
                case _ => None
            }
        }

        def parseElf(elf: (Int, Int),
            currElves: Set[(Int, Int)],
            currMoves: List[Move]):
                (Int, Int) =
        {
            if (!neighbourhood(elf).exists(t => currElves.contains(t)))
                elf
            else
            {
                val mt = findMove(currMoves, dir => !currElves.contains(
                    (elf._1 + dir._1, elf._2 + dir._2)), List())

                mt match {
                    case Some(move) => {
                        val dir = move.direction
                        (elf._1 + dir._1, elf._2 + dir._2)
                    }
                    case None => elf
                }
            }
        }

        @tailrec
        def performRound(currElves: Set[(Int, Int)], 
            currMoves: List[Move],
            currRound: Int): Set[(Int, Int)] =
        {
            if (currRound >= roundLimit)
                currElves
            else
            {
                val elfProposals = currElves.map(e => e -> parseElf(e, currElves, currMoves))
                val newPos = elfProposals.toList.map(_._2)
                    .groupBy(identity).mapValues(_.size)
                val newElves = elfProposals.map(e => if (newPos(e._2) > 1) e._1 else e._2)

                if (newElves == currElves)
                    currElves
                else
                    performRound(newElves, currMoves.tail :+ currMoves.head, currRound + 1)
            }
        }

        performRound(elves, moves, 0)
    }

    def main(args: Array[String]): Unit =
    {
        val elves = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.toList.zipWithIndex.collect({
                case ('#', x) => x
            }))
            .zipWithIndex
            .flatMap(t => t._1.map((_, t._2)))
            .toSet
        
        val roundCount = 10

        def movePre(posList: List[(Int, Int)]) = 
            (validator: ((Int, Int)) => Boolean) => posList.forall(validator)

        val moves = List(
            Move(movePre(List((0, -1), (1, -1), (-1, -1))), (0, -1)),
            Move(movePre(List((0, 1), (-1, 1), (1, 1))), (0, 1)),
            Move(movePre(List((-1, 0), (-1, -1), (-1, 1))), (-1, 0)),
            Move(movePre(List((1, 0), (1, -1), (1, 1))), (1, 0))
        )

        val spread = spreadElves(elves, moves, roundCount)

        val minX = spread.map(_._1).min
        val maxX = spread.map(_._1).max
        val minY = spread.map(_._2).min
        val maxY = spread.map(_._2).max

        println(((maxX - minX + 1) * (maxY - minY + 1)) - spread.size)
    }
}