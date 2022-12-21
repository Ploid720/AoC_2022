import scala.annotation.tailrec

object Puzzle2
{
    case class MonkeyOperation(left: String, op: (Long, Long) => Long, right: String,
        recreateLeft: (Long, Long) => Long, recreateRight: (Long, Long) => Long)

    def simplify(monkeyMap: Map[String, Either[Long, MonkeyOperation]],
        keep: Set[String]): 
        (Map[String, Long], Map[String, MonkeyOperation]) =
    {
        @tailrec
        def parse(monkeyVals: Map[String, Long], monkeyOps: Map[String, MonkeyOperation])
            : (Map[String, Long], Map[String, MonkeyOperation]) =
        {
            val parsed = monkeyOps.map(t => {
                val name = t._1
                val mop = t._2

                if (monkeyVals.contains(mop.left) 
                    && monkeyVals.contains(mop.right))
                    (name, Left(mop.op(monkeyVals(mop.left), monkeyVals(mop.right))))
                else
                    (name, Right(mop))
            })

            val mvs = monkeyVals ++ parsed.collect({
                case (name, Left(n)) => (name, n)
            })
            val mops = parsed.collect({
                case (name, Right(op)) => (name, op)
            })

            if (mops.isEmpty || (mops == monkeyOps))
            {
                val neededVals = mops.flatMap(t => List(t._2.left, t._2.right)).toSet ++ keep
                (mvs.filter(t => neededVals contains t._1), mops)
            }
            else
                parse(mvs, mops)
        }

        val mvs = monkeyMap.collect({
            case (name, Left(n)) => (name, n)
        })
        val mops = monkeyMap.collect({
            case (name, Right(op)) => (name, op)
        })
        parse(mvs, mops)
    }

    @tailrec
    def recreateValue(monkey: String, 
        monkeyVals: Map[String, Long], 
        monkeyOps: Map[String, MonkeyOperation]): Long =
    {
        val parsed = monkeyOps
            .map(t => {
                val name = t._1
                val mop = t._2

                if (monkeyVals.contains(name))
                {
                    if (monkeyVals.contains(mop.left))
                        Left(mop.right, mop.recreateRight(
                            monkeyVals(mop.left), monkeyVals(name)))
                    else if (monkeyVals.contains(mop.right))
                        Left(mop.left, mop.recreateLeft(
                            monkeyVals(mop.right), monkeyVals(name)))
                    else
                        throw new IllegalStateException(
                            s"Cannot reverse operation $name: no argument known")
                }
                else
                    Right(name, mop)
            })
        
        val mvs = monkeyVals ++ parsed.collect({
            case Left((name, n)) => (name, n)
        }).toMap

        if (mvs.contains(monkey))
            mvs(monkey)
        else
        {
            val mops = parsed.collect({
                case Right((name, op)) => (name, op)
            }).toMap

            if (mops.isEmpty || (mops == monkeyOps))
                throw new IllegalArgumentException(
                    s"Solution does not depend on $monkey; unsolvable")
            else
                recreateValue(monkey, mvs, mops)
        }
    }

    def main(args: Array[String]): Unit =
    {
        val numPat = "(\\d+)".r
        val opPat = "(\\w+)\\s*([\\+\\-\\*\\/])\\s*(\\w+)".r

        val monkeys = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(s => (s.takeWhile(_ != ':').trim, s.dropWhile(_ != ':').tail.trim))
            .map(s => (s._1, s._2 match {
                case numPat(n) => Left(n.toLong)
                case opPat(left, op, right) => Right(op match {
                    case "+" => MonkeyOperation(left, _ + _, right, 
                        (right, res) => res - right, (left, res) => res - left)
                    case "-" => MonkeyOperation(left, _ - _, right, 
                        (right, res) => res + right, (left, res) => left - res)
                    case "*" => MonkeyOperation(left, _ * _, right, 
                        (right, res) => res / right, (left, res) => res / left)
                    case "/" => MonkeyOperation(left, _ / _, right, 
                        (right, res) => res * right, (left, res) => left / res)
                    case _ => throw new IllegalArgumentException
                })
                case _ => throw new IllegalArgumentException
            }))
        val rawMonkeyMap = monkeys.toMap

        val root = "root"
        val me = "humn"

        val rootMop = rawMonkeyMap(root) match {
            case Left(_) => throw new IllegalArgumentException("Invalid root")
            case Right(mop) => mop
        }
        val rootLeft = rootMop.left
        val rootRight = rootMop.right

        val monkeyMaps = simplify(rawMonkeyMap - root - me, Set(rootLeft, rootRight))

        val monkeyVals = monkeyMaps._1
        val monkeyOps = monkeyMaps._2

        if (monkeyVals.contains(rootLeft))
        {
            if (monkeyVals.contains(rootRight))
            {
                val v1 = monkeyVals(rootLeft)
                val v2 = monkeyVals(rootRight)
                if (v1 == v2)
                    println(s"Root does not depend on $me; always true")
                else
                    println(s"Root does not depend on $me; unsolvable")
            }
            else
            {
                val v = monkeyVals(rootLeft)
                println(recreateValue(me, 
                    monkeyMaps._1 + (root -> v), 
                    monkeyMaps._2 + (root -> rootMop.copy(
                        recreateLeft = (a, b) => v,
                        recreateRight = (a, b) => v
                    ))))
            }
        }
        else
        {
            if (monkeyVals.contains(rootRight))
            {
                val v = monkeyVals(rootRight)
                println(recreateValue(me, 
                    monkeyMaps._1 + (root -> v), 
                    monkeyMaps._2 + (root -> rootMop.copy(
                        recreateLeft = (a, b) => v,
                        recreateRight = (a, b) => v
                    ))))
            }
            else
                throw new IllegalStateException(
                    s"Cannot reverse operation $root: no argument known")
        }
    }
}