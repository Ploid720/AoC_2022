
import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle1BadIdea
{
    case class Valve(name: String, flowRate: Int, tunnels: List[String])
    case class OpenValve(flowRate: Int, timeLeft: Int)

    def findBestPlan(start: String, valveMap: Map[String, Valve], time: Int): Int =
    {
        val posFlowRateValveCount = valveMap.values
            .filter(_.flowRate > 0)
            .size

        val valveList = valveMap.values
            .filter(_.flowRate > 0)
            .map(v => (v.name, v.flowRate))
            .toList
            .sortBy(_._2)(Ordering.Int.reverse)

        @tailrec
        def search(left: List[(List[(String, Boolean)], Int)],
            path: List[(String, Option[OpenValve], Int)],
            openValves: Set[String],
            score: Int,
            bestScore: Int): Int =
        {
            left match {
                case h +: t => {
                    val branches = h._1
                    val timeLeft = h._2

                    if ((timeLeft <= 0)
                        || (openValves.size >= posFlowRateValveCount)
                        || ((bestScore - score) >= valveList
                            .filter(t => !openValves.contains(t._1))
                            .take(timeLeft)
                            .zipWithIndex
                            .map(t => t._1._2 * (timeLeft - (t._2 * 2)))
                            .sum))
                    {
                        val bsc = Math.max(score, bestScore)
                        path.head match {
                            case (v, Some(_), sc) => search(t, path.tail, openValves - v, sc, bsc)
                            case (_, None, sc) => search(t, path.tail, openValves, sc, bsc)
                        }
                    }
                    else branches match {
                        case valveData :: bt => {
                            val valve = valveMap(valveData._1)
                            val valveName = valve.name
                            val opening = valveData._2
                            val alreadyOpen = openValves.contains(valveName)
                            
                            val nextBranches = valve.tunnels
                                .map((_, false))
                            if (!(opening || alreadyOpen) && (valve.flowRate > 0))
                                search(((valveName, true) :: nextBranches, timeLeft - 1) 
                                    :: (bt, timeLeft) 
                                    :: t, (valveName, None, score) :: path,
                                    openValves,
                                    score, 
                                    bestScore)
                            else if (opening && !alreadyOpen)
                                search((nextBranches, timeLeft - 1) 
                                    :: (bt, timeLeft) 
                                    :: t, (valveName, Some(OpenValve(valve.flowRate, timeLeft)), score) 
                                    :: path,
                                    openValves + valveName,
                                    score + (valve.flowRate * timeLeft),
                                    bestScore)
                            else
                                search((nextBranches, timeLeft - 1) 
                                    :: (bt, timeLeft) 
                                    :: t, (valveName, None, score) :: path,
                                    openValves,
                                    score,
                                    bestScore)
                        }
                        case _ => {
                            val bsc = Math.max(score, bestScore)
                            path.head match {
                                case (v, Some(_), sc) => search(t, path.tail, openValves - v, sc, bsc)
                                case (_, None, sc) => search(t, path.tail, openValves, sc, bsc)
                            }
                        }
                    }
                }
                case _ => bestScore
            }
        }

        search(List((List((start, false)), time)), List(("_", None, 0)), Set(), 0, 0)
    }

    def main(args: Array[String]): Unit =
    {
        val valvePat = "Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? ([\\w\\s,]+)".r

        val valveMap = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(_ match {
                case valvePat(name, flowRate, tunnels) => 
                    Valve(name, flowRate.toInt, tunnels.split(",").map(_.trim).toList)
                case _ => throw new IllegalArgumentException
            })
            .groupBy(_.name)
            .map(t => (t._1, t._2.head))

        val startValve = "AA"
        val time = 30

        val totalGasReleased = findBestPlan(startValve, valveMap, time)

        println(totalGasReleased)
    }
}