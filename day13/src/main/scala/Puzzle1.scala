import scala.annotation.tailrec

object Puzzle1
{
    case class PacketNode(content: List[Either[PacketNode, Int]])

    def buildTree(s: String): PacketNode =
    {
        @tailrec
        def parseTree(left: List[Char], stack: List[PacketNode], 
            acc: Option[Int]): PacketNode =
        {
            left match {
                case ch :: le => ch match {
                    case '[' => 
                        parseTree(le, PacketNode(List()) :: stack, acc)
                    case ']' => {
                        val head = acc match {
                            case Some(n) => {
                                val h = stack.head
                                val list = Right(n) :: h.content
                                h.copy(content = list.reverse)
                            }
                            case None => {
                                val h = stack.head
                                h.copy(content = h.content.reverse)
                            }
                        }
                        val currStack = stack.tail
                        val parent = currStack.head
                        val list = parent.content
                        parseTree(le, parent.copy(content = Left(head) :: list)
                            :: currStack.tail, None)
                    }
                    case ',' => {
                        acc match {
                            case Some(n) => {
                                val h = stack.head
                                val list = Right(n) :: h.content
                                parseTree(le, h.copy(content = list) :: stack.tail, None)
                            }
                            case None => parseTree(le, stack, None)
                        }
                    }
                    case c if (c.isDigit) => {
                        acc match {
                            case Some(n) => parseTree(le, stack, Some(n * 10 + c.asDigit))
                            case None => parseTree(le, stack, Some(c.asDigit))
                        }
                    }
                    case c => throw new IllegalArgumentException(s"Unknown character: $c")
                }
                case _ => stack.head.content.head match {
                    case Left(root) => root
                    case Right(_) => throw new IllegalArgumentException
                }
            }
        }

        parseTree(s.toList, List(PacketNode(List())), None)
    }
    def compareNodes(left: PacketNode, right: PacketNode): Boolean =
    {
        @tailrec
        def compareLists(l: List[Either[PacketNode, Int]],
            r: List[Either[PacketNode, Int]],
            lLeft: List[List[Either[PacketNode, Int]]],
            rLeft: List[List[Either[PacketNode, Int]]]): Boolean =
        {
            (l, r) match {
                case (h1 :: t1, h2 :: t2) =>
                {
                    (h1, h2) match {
                        case (Left(pn1), Left(pn2)) => 
                            compareLists(pn1.content, pn2.content,
                                t1 :: lLeft, t2 :: rLeft)
                        case (Left(pn1), Right(n2)) => 
                            compareLists(pn1.content, List(Right(n2)),
                                t1 :: lLeft, t2 :: rLeft)
                        case (Right(n1), Left(pn2)) => 
                            compareLists(List(Right(n1)), pn2.content,
                                t1 :: lLeft, t2 :: rLeft)
                        case (Right(n1), Right(n2)) => {
                            if (n1 < n2)
                                true
                            else if (n1 > n2)
                                false
                            else
                                compareLists(t1, t2, lLeft, rLeft)
                        }
                    }
                }
                case (h1 :: t1, _) => false
                case (_, h2 :: t2) => true
                case (_, _) => (lLeft, rLeft) match {
                    case (h1 :: t1, h2 :: t2) => compareLists(h1, h2, t1, t2)
                    case (h1 :: t1, _) => false
                    case (_, h2 :: t2) => true
                    case (_, _) => true
                }
            }
        }
        
        compareLists(left.content, right.content, List(), List())
    }

    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.trim)
            .filter(_.nonEmpty)
            .map(buildTree(_))
            .grouped(2)
            .map(_ match {
                case List(a, b) => (a, b)
                case _ => throw new IllegalArgumentException
            })
            .map(t => compareNodes(t._1, t._2))
            .zipWithIndex
            .filter(_._1 == true)
            .map(_._2 + 1)
            .sum)
    }
}