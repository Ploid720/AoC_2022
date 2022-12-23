import scala.annotation.tailrec

object Puzzle2
{
    trait FacingDirection
    {
        def rotate(r: Rotation): FacingDirection
    }

    case object FacingUp extends FacingDirection {
        override def rotate(r: Rotation): FacingDirection = r match {
            case Clockwise => FacingRight
            case Counterclockwise => FacingLeft
            case _ => throw new IllegalArgumentException
        }
    }
    case object FacingDown extends FacingDirection {
        override def rotate(r: Rotation): FacingDirection = r match {
            case Clockwise => FacingLeft
            case Counterclockwise => FacingRight
            case _ => throw new IllegalArgumentException
        }
    }
    case object FacingLeft extends FacingDirection {
        override def rotate(r: Rotation): FacingDirection = r match {
            case Clockwise => FacingUp
            case Counterclockwise => FacingDown
            case _ => throw new IllegalArgumentException
        }
    }
    case object FacingRight extends FacingDirection {
        override def rotate(r: Rotation): FacingDirection = r match {
            case Clockwise => FacingDown
            case Counterclockwise => FacingUp
            case _ => throw new IllegalArgumentException
        }
    }

    trait Rotation

    case object Clockwise extends Rotation
    case object Counterclockwise extends Rotation

    def mod(x: Int, y: Int) = x % y + (if (x < 0) y else 0)

    def followPath(path: List[Either[Int, Rotation]],
        map: Map[(Int, Int),Boolean],
        edgeLength: Int,
        portals: Map[(Int, Int, FacingDirection), (Int, Int, FacingDirection)],
        startPos: (Int, Int),
        startDir: FacingDirection): (Int, Int, FacingDirection) =
    {
        val faceMap = map
            .toList
            .map(t => (t, (t._1._1 / edgeLength, t._1._2 / edgeLength)))
            .groupBy(_._2)
            .mapValues(_.map(_._1))
            .toMap

        @tailrec
        def step(currStep: Int, stepTarget: Int, pos: (Int, Int), 
            dir: FacingDirection): ((Int, Int), FacingDirection) =
        {
            if (currStep > stepTarget)
                (pos, dir)
            else
            {
                assert(map contains pos)
                assert(map(pos))

                val posCand = dir match {
                    case FacingUp => (pos._1, pos._2 - 1)
                    case FacingDown => (pos._1, pos._2 + 1)
                    case FacingLeft => (pos._1 - 1, pos._2)
                    case FacingRight => (pos._1 + 1, pos._2)
                    case _ => throw new IllegalArgumentException
                }

                val npnd = if (map contains posCand)
                    (posCand, dir)
                else
                {
                    val faceX = pos._1 / edgeLength
                    val faceY = pos._2 / edgeLength
                    val relX = pos._1 % edgeLength
                    val relY = pos._2 % edgeLength

                    val portalPart = dir match {
                        case FacingUp => edgeLength - relX - 1
                        case FacingDown => relX
                        case FacingLeft => relY
                        case FacingRight => edgeLength - relY - 1
                        case _ => throw new IllegalArgumentException
                    }

                    val destination = portals((faceX, faceY, dir))

                    val baseX = destination._1 * edgeLength
                    val baseY = destination._2 * edgeLength
                    val newDir = destination._3

                    val newRelPos = newDir match {
                        case FacingUp => (edgeLength - portalPart - 1, edgeLength - 1)
                        case FacingDown => (portalPart, 0)
                        case FacingLeft => (edgeLength - 1, portalPart)
                        case FacingRight => (0, edgeLength - portalPart - 1)
                        case _ => throw new IllegalArgumentException
                    }

                    val newPos = (baseX + newRelPos._1, baseY + newRelPos._2)

                    (newPos, newDir)
                }

                val newPos = npnd._1
                val newDir = npnd._2

                assert(map contains newPos)

                if (!map(newPos))
                    (pos, dir)
                else
                    step(currStep + 1, stepTarget, newPos, newDir)
            }
        }
        @tailrec
        def parsePath(left: List[Either[Int, Rotation]], 
            pos: (Int, Int), dir: FacingDirection):
            (Int, Int, FacingDirection) =
        {
            left match {
                case Left(n) :: t => {
                    val parsed = step(1, n, pos, dir)
                    parsePath(t, parsed._1, parsed._2)
                }
                case Right(r) :: t => parsePath(t, pos, dir.rotate(r))
                case _ => (pos._1, pos._2, dir)
            }
        }

        parsePath(path, startPos, startDir)
    }

    def main(args: Array[String]): Unit =
    {
        val input = io.Source
            .fromResource("input.txt")
            .getLines
            .toList

        val map = input
            .takeWhile(_.nonEmpty)
            .zipWithIndex
            .flatMap(t => t._1.toList
                .zipWithIndex
                .flatMap(e => e._1 match {
                    case '.' => Some((e._2, t._2) -> true)
                    case '#' => Some((e._2, t._2) -> false)
                    case _ => None
                }))
            .toMap

        val path = input
                .dropWhile(_.nonEmpty)
                .tail
                .head
                .toList
                .foldLeft(List[Either[Int, Rotation]]())((z, a) => a match {
                    case d if d.isDigit => z match {
                        case Left(n) :: t => Left(10 * n + d.asDigit) :: t
                        case _ => Left(d.asDigit) :: z
                    }
                    case 'L' => Right(Counterclockwise) :: z
                    case 'R' => Right(Clockwise) :: z
                    case _ => throw new IllegalArgumentException
                })
                .reverse

        val edgeLength = Math.sqrt(map.size / 6).toInt

        //Hard-coded net layout. Might come back to this later
        val portals: Map[(Int, Int, FacingDirection), 
            (Int, Int, FacingDirection)] = 
        Map((1, 0, FacingUp) -> (0, 3, FacingRight),
            (2, 0, FacingUp) -> (0, 3, FacingUp),
            (1, 0, FacingLeft) -> (0, 2, FacingRight),
            (2, 0, FacingRight) -> (1, 2, FacingLeft),
            (2, 0, FacingDown) -> (1, 1, FacingLeft),
            (1, 1, FacingLeft) -> (0, 2, FacingDown),
            (1, 1, FacingRight) -> (2, 0, FacingUp),
            (0, 2, FacingUp) -> (1, 1, FacingRight),
            (0, 2, FacingLeft) -> (1, 0, FacingRight),
            (1, 2, FacingRight) -> (2, 0, FacingLeft),
            (1, 2, FacingDown) -> (0, 3, FacingLeft),
            (0, 3, FacingLeft) -> (1, 0, FacingDown),
            (0, 3, FacingRight) -> (1, 2, FacingUp),
            (0, 3, FacingDown) -> (2, 0, FacingDown))

        val startPos = map.foldLeft(None: Option[(Int, Int)])((z, a) => Some(z match {
                case None => a._1
                case Some(p) => {
                    if (a._1._2 < p._2)
                        a._1
                    else if ((a._1._2 == p._2) && (a._1._1 < p._1))
                        a._1
                    else
                        p
                }
            })).get
        val startDir: FacingDirection = FacingRight

        val finalState = followPath(path, map, edgeLength, portals, startPos, startDir)

        println(1000 * (finalState._2 + 1) + 
            4 * (finalState._1 + 1) +
            (finalState._3 match {
                case FacingUp => 3
                case FacingDown => 1
                case FacingLeft => 2
                case FacingRight => 0
                case _ => throw new IllegalArgumentException
            }))
    }
}