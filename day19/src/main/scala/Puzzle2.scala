import scala.annotation.tailrec

object Puzzle2
{
    def collectGeo(blueprint: List[(String, List[(String, Int)])],
        time: Int,
        startRobots: Map[String, Int],
        startResources: Map[String, Int]): Int =
    {
        val blueprintMap = blueprint.map(t => (t._1, t._2.toMap)).toMap

        val maxResourceMap = blueprint
            .flatMap(_._2)
            .groupBy(_._1)
            .map(t => (t._1, t._2
                .map(_._2)
                .max))

        var typeCount = blueprint.size

        @tailrec
        def dfs(stack: List[List[(Map[String, Int], Map[String, Int], Set[String])]],
            timeLeft: Int, bestGeoScore: Int): Int =
        {
            stack match {
                case sh :: st => {
                    sh match {
                        case h :: t => {
                            if (timeLeft <= 0)
                            {
                                val score = h._2
                                    .find(_._1 == "geode")
                                    .map(_._2)
                                    .getOrElse(0)
                                if (score > bestGeoScore)
                                    dfs(st, timeLeft + 1, score)
                                else
                                    dfs(st, timeLeft + 1, bestGeoScore)
                            }
                            else
                            {
                                val robots = h._1
                                val resources = h._2

                                val scoreDiff = bestGeoScore - resources
                                    .find(_._1 == "geode")
                                    .map(_._2)
                                    .getOrElse(0)
                                
                                val maxGeoGain = (timeLeft - 1) * timeLeft / 2
                                val maxGeo = (timeLeft * robots.getOrElse("geode", 0)) + maxGeoGain
                                if ((scoreDiff >= maxGeo)
                                    || robots.exists(r => (r._1 != "geode") 
                                        && (r._2 > maxResourceMap(r._1))))
                                    dfs(st, timeLeft + 1, bestGeoScore)
                                else
                                {
                                    val robotsToSkip = h._3
                                    val futureResources = robots.keySet
                                        .union(resources.keySet)
                                        .map(k => k -> (resources.getOrElse(k, 0) + robots.getOrElse(k, 0)))
                                        .toMap

                                    val buildCandidates = blueprint
                                        .filter(rcd => !robotsToSkip.contains(rcd._1))
                                        .map(rcd => (rcd._1, rcd._2
                                            .filter(_._2 > 0)
                                            .map(cd => resources.getOrElse(cd._1, 0) / cd._2)
                                            .min))
                                        .filter(_._2 > 0)
                                        .map(_._1)

                                    val basePaths = buildCandidates
                                            .toVector
                                            .map(rb => (robots + 
                                                (rb -> (robots.getOrElse(rb, 0) + 1)),
                                                futureResources
                                                    .map(r => (r._1, r._2
                                                        - blueprintMap(rb).getOrElse(r._1, 0))),
                                                Set[String]()))
                                    val paths = (if (basePaths.length < typeCount) 
                                        (basePaths :+ (robots, futureResources, buildCandidates.toSet))
                                        else basePaths).toList

                                    dfs(paths :: t :: st, timeLeft - 1, bestGeoScore)
                                }
                            }
                        }
                        case _ => dfs(st, timeLeft + 1, bestGeoScore)
                    }
                }
                case _ => bestGeoScore
            }
        }

        dfs(List(List((startRobots, startResources, Set()))), time, 0)
    }

    def main(args: Array[String]): Unit =
    {
        val blueprintPat = "Blueprint (\\d+)".r
        val robotPat = ":?\\s*Each (\\w+) robot costs ([\\w\\d\\s]+)".r
        val costPat = "(\\d+) (\\w+)".r

        val materialValueMap = Map(
            "geode" -> 4,
            "obsidian" -> 3,
            "clay" -> 2,
            "ore" -> 1
        )

        val blueprints = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(s => (s.takeWhile(_ != ':'), s.dropWhile(_ != ':').split("\\.")))
            .map(bp => (bp._1 match {
                    case blueprintPat(id) => id.toInt
                    case _ => throw new IllegalArgumentException
                },
                bp._2.map(_.trim match {
                    case robotPat(robotType, costs) => (robotType, costs)
                    case _ => throw new IllegalArgumentException
                }).toList
                .map(t => (t._1, materialValueMap.getOrElse(t._1, 0), t._2
                    .split("and")
                    .map(_.trim match {
                        case costPat(cost, materialType) => (materialType, cost.toInt)
                        case _ => throw new IllegalArgumentException
                    }).toList))
                .sortBy(_._2)(Ordering.Int.reverse)
                .map(t => (t._1, t._3))))
            .take(3)

        val time = 32

        println(blueprints
            .map(bp => collectGeo(bp._2, time, Map("ore" -> 1), Map()))
            .product)
    }
}