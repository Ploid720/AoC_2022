import scala.annotation.tailrec

object Puzzle1
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
        startPos: (Int, Int),
        startDir: FacingDirection): (Int, Int, FacingDirection) =
    {
        val verBounds = map.groupBy(_._1._1)
            .map(t => (t._1, (t._2.map(_._1._2).min, t._2.map(_._1._2).max)))
            .map(t => (t._1, (t._2._1, t._2._2 - t._2._1 + 1)))
            .toMap
        val horBounds = map.groupBy(_._1._2)
            .map(t => (t._1, (t._2.map(_._1._1).min, t._2.map(_._1._1).max)))
            .map(t => (t._1, (t._2._1, t._2._2 - t._2._1 + 1)))
            .toMap

        def createCoordMapper(pos: (Int, Int), dir: FacingDirection):
            (Int => (Int, Int)) =
        {
            dir match {
                case FacingUp => {
                    val x = pos._1
                    val y = pos._2
                    val bounds = verBounds(x)
                    val base = bounds._1
                    val range = bounds._2
                    n => (x, mod(y - n - base, range) + base)
                }
                case FacingDown => {
                    val x = pos._1
                    val y = pos._2
                    val bounds = verBounds(x)
                    val base = bounds._1
                    val range = bounds._2
                    n => (x, mod(y + n - base, range) + base)
                }
                case FacingLeft => {
                    val x = pos._1
                    val y = pos._2
                    val bounds = horBounds(y)
                    val base = bounds._1
                    val range = bounds._2
                    n => (mod(x - n - base, range) + base, y)
                }
                case FacingRight => {
                    val x = pos._1
                    val y = pos._2
                    val bounds = horBounds(pos._2)
                    val base = bounds._1
                    val range = bounds._2
                    n => (mod(x + n - base, range) + base, y)
                }
                case _ => throw new IllegalArgumentException
            }
        }
        @tailrec
        def step(currStep: Int, stepTarget: Int, lastPos: (Int, Int), 
            coordMapper: Int => (Int, Int)): (Int, Int) =
        {
            if (currStep > stepTarget)
                lastPos
            else
            {
                val pos = coordMapper(currStep)

                if (!map(pos))
                    lastPos
                else
                    step(currStep + 1, stepTarget, pos, coordMapper)
            }
        }
        @tailrec
        def parsePath(left: List[Either[Int, Rotation]], 
            pos: (Int, Int), dir: FacingDirection):
            (Int, Int, FacingDirection) =
        {
            left match {
                case Left(n) :: t => parsePath(t, 
                    step(1, n, pos, createCoordMapper(pos, dir)), dir)
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

        val finalState = followPath(path, map, startPos, startDir)
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