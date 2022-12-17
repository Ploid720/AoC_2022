
import collection.immutable.Queue
import scala.annotation.tailrec

object Puzzle1
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
        
        val vList = valveList.map(_._1)
        val vListWithStart = (start :: vList).toSet.toList

        @tailrec
        def findPaths(pathStart: String, left: Queue[(String, List[String])],
            visited: Set[String], paths: Map[String, Int]): Map[String, Int] =
        {
            left match {
                case h +: t => {
                    val valveName = h._1
                    if (visited.contains(valveName))
                        findPaths(pathStart, t, visited, paths)
                    else
                    {
                        val pathBase = h._2
                        val path = valveName :: pathBase
                        val valve = valveMap(valveName)
                        val nb = valve.tunnels
                            .filter(v => !visited.contains(v))
                        if (valve.flowRate > 0)
                            findPaths(pathStart, t ++ nb.map((_, path)),
                                visited + valveName, paths + (valveName -> (path.size - 1)))
                        else
                            findPaths(pathStart, t ++ nb.map((_, path)),
                                visited + valveName, paths)
                    }
                }
                case _ => paths
            }
        }
        val distances = vListWithStart
            .map(v => (v, findPaths(v, Queue((v, List())), Set(), Map())))
            .toMap
        
        @tailrec
        def search(left: List[(List[(String, Int)], Int)],
            path: List[(String, Option[OpenValve], Int)],
            openValves: Set[String],
            score: Int,
            bestScore: Int): Int =
        {
            left match {
                case h +: t => {
                    val branches = h._1
                    val timeBase = h._2

                    if ((timeBase <= 0)
                        || ((bestScore - score) >= valveList
                            .filter(t => !openValves.contains(t._1))
                            .take(timeBase)
                            .zipWithIndex
                            .map(t => t._1._2 * (timeBase - (t._2 * 2)))
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
                            val timeLeft = timeBase - valveData._2

                            val valve = valveMap(valveData._1)
                            val valveName = valve.name
                            val nextBranches = vList.filter(v => 
                                !((v == valveName) || openValves.contains(v)))
                                .map(v => (v, distances(valveName)(v)))

                            if (valve.flowRate > 0)
                            {
                                val tl = timeLeft - 1
                                search((nextBranches, tl) 
                                    :: (bt, timeBase)
                                    :: t, (valveName, Some(OpenValve(valve.flowRate, tl)), score) 
                                    :: path,
                                    openValves + valveName,
                                    score + (valve.flowRate * tl),
                                    bestScore)
                            }
                            else
                                search((nextBranches, timeLeft) 
                                    :: (bt, timeBase) 
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

        search(List((List((start, 0)), time)), List(("_", None, 0)), Set(), 0, 0)
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