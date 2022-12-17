
import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle2
{
    def leftEdge(rock: Set[(Int, Int)]) = rock.map(_._1).min
    def rightEdge(rock: Set[(Int, Int)]) = rock.map(_._1).max
    def topEdge(rock: Set[(Int, Int)]) = rock.map(_._2).min
    def bottomEdge(rock: Set[(Int, Int)]) = rock.map(_._2).max

    case class SimulationState(rock: Set[(Int, Int)], rockStack: Set[(Int, Int)],
        rockOrderLeft: List[Char], windLeft: List[Char], rockCount: Int,
        rockStackHeight: Int, rockLeftEdges: Map[Char, Int],
        rockBottomEdges: Map[Char, Int], stackHeight: Int,
        rockTypes: Map[Char, Set[(Int, Int)]], rockOrder: List[Char],
        rockTotalCount: Int, wind: List[Char], width: Int,
        leftMargin: Int, bottomMargin: Int)
    
    def prepareSimulationState(rockTypes: Map[Char, Set[(Int, Int)]],
        rockOrder: List[Char], rockTotalCount: Int,
        wind: List[Char], width: Int, 
        leftMargin: Int, bottomMargin: Int,
        rockBase: Set[(Int, Int)]): SimulationState =
    {
        val rockLeftEdges = rockTypes.map(t => (t._1, leftEdge(t._2)))
        val rockBottomEdges = rockTypes.map(t => (t._1, bottomEdge(t._2)))
        val stackHeight = if (rockBase.isEmpty) 0 else -topEdge(rockBase)
        SimulationState(
            spawnRock(rockOrder.head, stackHeight, rockLeftEdges, 
                rockBottomEdges, rockTypes, leftMargin, bottomMargin), 
                rockBase, rockOrder.tail, wind, 1, stackHeight,
                rockLeftEdges, rockBottomEdges, stackHeight,
                rockTypes, rockOrder, rockTotalCount, wind, width, 
                leftMargin, bottomMargin)
    }

    def spawnRock(rockType: Char, rockStackHeight: Int,
        rockLeftEdges: Map[Char, Int],
        rockBottomEdges: Map[Char, Int],
        rockTypes: Map[Char, Set[(Int, Int)]],
        leftMargin: Int, 
        bottomMargin: Int): Set[(Int, Int)] =
    {
        val leftEdge = rockLeftEdges(rockType)
        val bottomEdge = rockBottomEdges(rockType)

        rockTypes(rockType)
            .map(t => (t._1 + leftMargin - leftEdge, 
                t._2 - rockStackHeight - bottomMargin - bottomEdge - 1))
    }
    @tailrec
    def processRocks(state: SimulationState): (Set[(Int, Int)], Int, SimulationState) =
    {
        val rock = state.rock
        val rockStack = state.rockStack
        val rockOrderLeft = state.rockOrderLeft
        val windLeft = state.windLeft
        val rockCount = state.rockCount
        val rockStackHeight = state.rockStackHeight

        val rockLeftEdges = state.rockLeftEdges
        val rockBottomEdges = state.rockBottomEdges
        val stackHeight = state.stackHeight

        val rockTypes = state.rockTypes
        val rockOrder = state.rockOrder
        val rockTotalCount = state.rockTotalCount
        val wind = state.wind
        val width = state.width
        val leftMargin = state.leftMargin
        val bottomMargin = state.bottomMargin

        if (rockCount > rockTotalCount)
            (rockStack.filter(t => t._2 < (20 - rockStackHeight)), rockStackHeight, state)
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
                        val newRockStackHeight = Math.max(-topEdge(validatedRock), rockStackHeight)
                        val newRockStack = rockStack ++ validatedRock

                        processRocks(SimulationState(
                            spawnRock(rockType, newRockStackHeight,
                                rockLeftEdges, rockBottomEdges, rockTypes, 
                                leftMargin, bottomMargin),
                            newRockStack, rol.tail, t, rockCount + 1, newRockStackHeight,
                            rockLeftEdges, rockBottomEdges, 
                            stackHeight, rockTypes, rockOrder, 
                            rockTotalCount, wind, width, 
                            leftMargin, bottomMargin))
                    }
                    else
                        processRocks(SimulationState(
                            fallenRock, rockStack, rockOrderLeft,
                            t, rockCount, rockStackHeight,
                            rockLeftEdges, rockBottomEdges, 
                            stackHeight, rockTypes, rockOrder, 
                            rockTotalCount, wind, width, 
                            leftMargin, bottomMargin))
                }
                case _ => processRocks(SimulationState(
                            rock, rockStack, rockOrderLeft, 
                            wind, rockCount, rockStackHeight,
                            rockLeftEdges, rockBottomEdges, 
                            stackHeight, rockTypes, rockOrder, 
                            rockTotalCount, wind, width, leftMargin, 
                            bottomMargin))
            }
        }
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
        val rockCount = 1000000000000l
        val wind = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .flatMap(_.toList)
        
        @tailrec
        def searchForMagicNumber(num: Int, state1: SimulationState, state2: SimulationState): Int =
        {
            def normalizedEquals(s1: Set[(Int, Int)], s2: Set[(Int, Int)]): Boolean =
            {
                val s1xOff = s1.map(_._1).min
                val s1yOff = s1.map(_._2).min
                val s2xOff = s2.map(_._1).min
                val s2yOff = s2.map(_._2).min

                s1.map(t => (t._1 - s1xOff, t._2 - s1yOff)) ==
                s2.map(t => (t._1 - s2xOff, t._2 - s2yOff)) 
            }

            val st1 = state1.copy(rockTotalCount = num)
            val st2 = state2.copy(rockTotalCount = 2 * num)

            val res1 = processRocks(st1)
            val res2 = processRocks(st2)

            if (normalizedEquals(res1._1, res2._1))
                num
            else
                searchForMagicNumber(num + rockOrder.size, res1._3, res2._3)
        }
        def serchForCycleLength(): Int =
        {
            val num = rockOrder.size

            val state1 = prepareSimulationState(rockTypes, rockOrder, num,
                wind, width, leftMargin, bottomMargin, Set())
            val state2 = prepareSimulationState(rockTypes, rockOrder, 2 * num,
                wind, width, leftMargin, bottomMargin, Set())

            searchForMagicNumber(num, state1, state2)
        }

        val cycleLength = serchForCycleLength()
        val cycleCount = rockCount / cycleLength
        val partialCycleLength = (rockCount % cycleLength).toInt

        val cRes = processRocks(prepareSimulationState(rockTypes, rockOrder, cycleLength,
            wind, width, leftMargin, bottomMargin, Set()))
        val c1Res = processRocks(prepareSimulationState(rockTypes, rockOrder, 2 * cycleLength,
            wind, width, leftMargin, bottomMargin, Set()))
        val pRes = processRocks(prepareSimulationState(rockTypes, rockOrder, (2 * cycleLength) + partialCycleLength,
            wind, width, leftMargin, bottomMargin, Set()))
        
        println((cycleCount - 2) * (c1Res._2 - cRes._2) + pRes._2)
    }
}