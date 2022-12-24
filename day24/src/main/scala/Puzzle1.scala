import scala.annotation.tailrec
import collection.immutable.Queue

object Puzzle1
{
    case class Wall(pos: (Int, Int))
    {
        def x = pos._1
        def y = pos._2
    }
    case class Blizzard(pos: (Int, Int), dir: (Int, Int))
    {
        def x = pos._1
        def y = pos._2
        def dirX = dir._1
        def dirY = dir._2
    }

    def solveMaze(walls: List[Wall], blizzards: List[Blizzard],
        minX: Int, maxX: Int, minY: Int, maxY: Int,
        start: (Int, Int), end: (Int, Int)): Option[Int] =
    {
        val wallPosSet = walls.map(_.pos).toSet

        val stepOffs = List((1,  0), (0,  1), (0, -1), (-1,  0), (0, 0))
        def steps(pos: (Int, Int)) = stepOffs.map(off => (pos._1 + off._1, pos._2 + off._2))

        @tailrec
        def advanceBlizzards(left: List[Blizzard], parsed: List[Blizzard]):
            List[Blizzard] =
        {
            left match {
                case h :: t => {
                    val nextBliz = Blizzard((h.x + h.dirX, h.y + h.dirY), h.dir)
                    if (wallPosSet.contains(nextBliz.pos))
                    {
                        val wrappedBliz = h.dir match {
                            case (0, -1) => Blizzard((h.x, maxY - 1), h.dir)
                            case (1, 0) => Blizzard((minX + 1, h.y), h.dir)
                            case (0, 1) => Blizzard((h.x, minY + 1), h.dir)
                            case (-1, 0) => Blizzard((maxX - 1, h.y), h.dir)
                            case _ => throw new IllegalArgumentException("Invalid direction")
                        }

                        advanceBlizzards(t, wrappedBliz :: parsed)
                    }
                    else
                        advanceBlizzards(t, nextBliz :: parsed)
                }
                case _ => parsed.reverse
            }
        }

        @tailrec
        def flood(turn: Int, posSet: Set[(Int, Int)], 
            blizzards: List[Blizzard]): 
            Option[Int] =
        {
            if (posSet.isEmpty)
                None
            else if (posSet.contains(end))
                Some(turn)
            else
            {
                val nextTurn = turn + 1
                val nextBlizzards = advanceBlizzards(blizzards, List())
                val blockedSpots = nextBlizzards.map(_.pos).toSet

                val nextPosSet = posSet
                    .flatMap(p => steps(p))
                    .filter(p => !blockedSpots.contains(p)
                        && !wallPosSet.contains(p)
                        && (p._1 >= minX)
                        && (p._1 <= maxX)
                        && (p._2 >= minY)
                        && (p._2 <= maxY))

                flood(nextTurn, nextPosSet, nextBlizzards)
            }
        }

        flood(0, Set(start), blizzards)
    }

    def main(args: Array[String]): Unit =
    {
        val maze = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .zipWithIndex
            .flatMap(t => t._1.toList
                .zipWithIndex
                .map(e => e._1 match {
                    case '#' => Wall((e._2, t._2))
                    case '^' => Blizzard((e._2, t._2), (0, -1))
                    case '>' => Blizzard((e._2, t._2), (1, 0))
                    case 'v' => Blizzard((e._2, t._2), (0, 1))
                    case '<' => Blizzard((e._2, t._2), (-1, 0))
                    case '.' => None
                    case _ => throw new IllegalArgumentException
                })
                .collect({
                    case Wall(pos) => Left(Wall(pos))
                    case Blizzard(pos, dir) => Right(Blizzard(pos, dir))
                }))

        val walls = maze.collect({case Left(w) => w})
        val blizzards = maze.collect({case Right(b) => b})

        val minX = walls.map(_.x).min
        val maxX = walls.map(_.x).max
        val minY = walls.map(_.y).min
        val maxY = walls.map(_.y).max

        val start = ((minX to maxX)
            .find(x => !walls.exists(w => 
                ((w.x == x) && (w.y == minY))))
            .get, minY)
        val end = ((minX to maxX)
            .find(x => !walls.exists(w => 
                ((w.x == x) && (w.y == maxY))))
            .get, maxY)

        solveMaze(walls, blizzards, minX, maxX, minY, maxY, start, end) match {
            case Some(time) => println(time)
            case None => println("Maze is unsolvable")
        }
    }
}