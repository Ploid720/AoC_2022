import scala.annotation.tailrec

object Puzzle1
{
    case class MonkeyOperation(left: String, op: (Long, Long) => Long, right: String)

    @tailrec
    def findValue(monkey: String, monkeyMap: Map[String, Either[Long, MonkeyOperation]]): Long =
    {
        monkeyMap(monkey) match {
            case Left(n) => n
            case Right(n) => findValue(monkey, monkeyMap
                .map(t => (t._1, t._2 match {
                    case Right(MonkeyOperation(left, op, right)) => 
                        (monkeyMap(left), monkeyMap(right)) match {
                            case (Left(lv), Left(rv)) => Left(op(lv, rv))
                            case _ => Right(MonkeyOperation(left, op, right))
                        }
                    case v => v
                })))
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
                case opPat(left, op, right) => Right(MonkeyOperation(left, op match {
                    case "+" => _ + _
                    case "-" => _ - _
                    case "*" => _ * _
                    case "/" => _ / _
                    case _ => throw new IllegalArgumentException
                }, right))
                case _ => throw new IllegalArgumentException
            }))
        val monkeyMap = monkeys.toMap

        println(findValue("root", monkeyMap))
    }
}