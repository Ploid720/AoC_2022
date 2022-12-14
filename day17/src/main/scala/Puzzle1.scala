
import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle1
{
    def simulateRocks(rockTypes: Map[Char, Set[(Int, Int)]],
        rockOrder: List[Char], rockTotalCount: Int,
        wind: List[Char], width: Int, 
        leftMargin: Int, bottomMargin: Int): Int =
    {
        def leftEdge(rock: Set[(Int, Int)]) = rock.map(_._1).min
        def rightEdge(rock: Set[(Int, Int)]) = rock.map(_._1).max
        def topEdge(rock: Set[(Int, Int)]) = rock.map(_._2).min
        def bottomEdge(rock: Set[(Int, Int)]) = rock.map(_._2).max

        val rockLeftEdges = rockTypes.map(t => (t._1, leftEdge(t._2)))
        val rockBottomEdges = rockTypes.map(t => (t._1, bottomEdge(t._2)))

        def spawnRock(rockType: Char, rockStackHeight: Int): Set[(Int, Int)] =
        {
            val leftEdge = rockLeftEdges(rockType)
            val bottomEdge = rockBottomEdges(rockType)

            rockTypes(rockType)
                .map(t => (t._1 + leftMargin - leftEdge, 
                    t._2 - rockStackHeight - bottomMargin - bottomEdge - 1))
        }
        @tailrec
        def processRock(rock: Set[(Int, Int)], rockStack: Set[(Int, Int)],
            rockOrderLeft: List[Char], windLeft: List[Char], rockCount: Int,
            rockStackHeight: Int): Int =
        {
            if (rockCount > rockTotalCount)
                rockStackHeight
            else
            {
                windLeft match {
                    case h :: t => {
                        val movedRock = rock.map(t => if (h == '<') 
                            (t._1 - 1, t._2) else (t._1 + 1, t._2))
                        val validatedRock = if ((leftEdge(movedRock) < 0) || 
                            (rightEdge(movedRock) >= width) ||
                            movedRock.exists(rockStack.contains))
                            rock else movedRock
                        val fallenRock = validatedRock.map(t => (t._1, t._2 + 1))
                        if (fallenRock.exists(rockStack.contains)
                            || (bottomEdge(fallenRock) >= 0))
                        {
                            val rol = if (rockOrderLeft.isEmpty) rockOrder else rockOrderLeft
                            val rockType = rol.head
                            val newRockStack = rockStack ++ validatedRock
                            val newRockStackHeight = Math.max(-topEdge(validatedRock), rockStackHeight)

                            processRock(spawnRock(rockType, newRockStackHeight),
                                newRockStack, rol.tail, t, rockCount + 1, newRockStackHeight)
                        }
                        else
                            processRock(fallenRock, rockStack, rockOrderLeft,
                                t, rockCount, rockStackHeight)
                    }
                    case _ => processRock(rock, rockStack, rockOrderLeft, 
                            wind, rockCount, rockStackHeight)
                }
            }
        }

        processRock(spawnRock(rockOrder.head, 0), Set(), rockOrder.tail, wind, 1, 0)
    }

    def main(args: Array[String]): Unit =
    {
        val rockTypes = Map(
            '-' -> Set((0, 0), (1, 0), (2, 0), (3, 0)),
            '+' -> Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)),
            'L' -> Set((2, 0), (2, 1), (0, 2), (1, 2), (2, 2)),
            '|' -> Set((0, 0), (0, 1), (0, 2), (0, 3)),
            'o' -> Set((0, 0), (1, 0), (0, 1), (1, 1))
        )
        val rockOrder = List('-', '+', 'L', '|', 'o')
        val width = 7
        val leftMargin = 2
        val bottomMargin = 3
        val rockCount = 2022
        val wind = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .flatMap(_.toList)
        
        val res = simulateRocks(rockTypes, rockOrder, rockCount,
            wind, width, leftMargin, bottomMargin)

        println(res)
    }
}