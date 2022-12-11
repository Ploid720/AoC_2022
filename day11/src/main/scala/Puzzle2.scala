import collection.immutable.Queue

object Puzzle2
{
    case class Monkey(id: Long, items: Queue[Long], 
        op: Long => Long, divisor: Long, test: Long => Boolean, 
        tCase: Long, fCase: Long)

    def parseParam(param: String): Long => Long =
    {
        val intPat = "(\\d+)".r

        param match {
            case intPat(n) => _ => n.toInt
            case "old" => v => v
            case _ => throw new IllegalArgumentException()
        }
    }
    def makeOp(left: Long => Long, right: Long => Long, op: (Long, Long) => Long): Long => Long =
        v => op(left(v), right(v))

    def performRounds(monkeys: List[Monkey], mod: Long, roundCount: Long): List[(Long, Long)] =
    {
        def parseMonkey(m: Monkey, passedItems: List[(Long, Long)]): (Long, List[(Long, Long)]) =
        {
            val id = m.id

            def receiveItems(left: List[(Long, Long)], received: List[(Long, Long)], 
                queued: List[(Long, Long)]): (List[(Long, Long)], List[(Long, Long)]) =
            {
                left match {
                    case h :: t if (h._1 == id) => receiveItems(t, h :: received, queued)
                    case h :: t => receiveItems(t, received, h :: queued)
                    case _ => (received.reverse, queued.reverse)
                }
            }
            val groupedItems = receiveItems(passedItems, List(), List())
            val items: Queue[Long] = m.items ++ groupedItems._1.map(_._2)

            def inspectItems(left: Queue[Long], passed: List[(Long, Long)]): List[(Long, Long)] =
            {
                left match {
                    case h +: t => {
                        val wl = m.op(h) % mod
                        val b = m.test(wl)
                        val pId = if (b) m.tCase else m.fCase
                        
                        inspectItems(t, (pId, wl) :: passed)
                    }
                    case _ => passed.reverse
                }
            }
            val newPassedItems = inspectItems(items, List())

            (items.size, groupedItems._2 ++ newPassedItems)
        }
        def performRound(round: Long, monkeysLeft: List[Monkey],
            monkeysParsed: List[Monkey], passedItems: List[(Long, Long)],
            inspectCounts: List[(Long, Long)]): List[(Long, Long)] =
        {
            if (round >= roundCount)
                inspectCounts
                    .groupBy(t => t._1)
                    .toList
                    .map(t => (t._1, t._2.map(_._2).reduce(_ + _)))
            else
            {
                monkeysLeft match {
                    case h :: t => {
                        val res = parseMonkey(h, passedItems)
                        val inspectCount = res._1
                        val newPassedItems = res._2
                        
                        performRound(round, t, h.copy(items=Queue()) :: monkeysParsed,
                            newPassedItems, (h.id, inspectCount) :: inspectCounts)
                    }
                    case _ => performRound(round + 1, monkeysParsed.reverse,
                        List(), passedItems, inspectCounts)
                }
            }
        }

        performRound(0, monkeys.reverse, List(), List(), List())
    }

    def main(args: Array[String]): Unit =
    {
        val monkeyPat = "Monkey (\\d+):".r
        val itemsPat = "Starting items: ([\\d,\\s]+)".r
        val opPat = "Operation: new = ([\\w\\s\\+\\-\\*\\/]+)".r
        val testPat = "Test: divisible by (\\d+)".r
        val tCasePat = "If true: throw to monkey (\\d+)".r
        val fCasePat = "If false: throw to monkey (\\d+)".r

        val addOpPat = "(\\w+) \\+ (\\w+)".r
        val subOpPat = "(\\w+) \\- (\\w+)".r
        val mulOpPat = "(\\w+) \\* (\\w+)".r
        val divOpPat = "(\\w+) \\/ (\\w+)".r

        val monkeys = io.Source
            .fromResource("input.txt")
            .getLines
            .map(_.trim)
            .foldLeft(List[Monkey]())((z, s) => s match {
                case monkeyPat(id) => Monkey(id.toInt, Queue(), v => v, 0, _ => true, 0, 0) :: z
                case itemsPat(itStr) => z.head.copy(items = Queue(itStr
                    .split(",")
                    .map(_.trim.toLong)
                    .toSeq: _*)) :: z.tail
                case opPat(opStr) => opStr match {
                    case addOpPat(left, right) => z.head.copy(op = 
                        makeOp(parseParam(left), parseParam(right), _ + _)) :: z.tail
                    case subOpPat(left, right) => z.head.copy(op = 
                        makeOp(parseParam(left), parseParam(right), _ - _)) :: z.tail
                    case mulOpPat(left, right) => z.head.copy(op = 
                        makeOp(parseParam(left), parseParam(right), _ * _)) :: z.tail
                    case divOpPat(left, right) => z.head.copy(op = 
                        makeOp(parseParam(left), parseParam(right), _ / _)) :: z.tail
                    case _ => throw new IllegalArgumentException(s"Unknown operation: $opStr")
                }
                case testPat(div) => z.head.copy(test = n => (n.toInt % div.toInt == 0),
                    divisor = div.toInt) :: z.tail
                case tCasePat(id) => z.head.copy(tCase = id.toInt) :: z.tail
                case fCasePat(id) => z.head.copy(fCase = id.toInt) :: z.tail
                case "" => z
                case _ => throw new IllegalArgumentException(s"Invalid instruction: $s")
            })

        val mod = monkeys
            .map(m => m.divisor)
            .reduce(_ * _)

        println(performRounds(monkeys, mod, 10000)
            .map(t => t._2)
            .sorted(Ordering.Long.reverse)
            .take(2)
            .reduce(_ * _))
    }
}