import scala.annotation.tailrec

object Puzzle1
{
    case class Tree(height: Int, left: Boolean, right: Boolean, top: Boolean, bottom: Boolean)

    def scanTreeLineFromLeft(trees: List[Tree],
        encode: (Tree, Boolean) => Tree): List[Tree] =
    {
        @tailrec
        def scan(left: List[Tree], parsed: List[Tree], maxSoFar: Int): List[Tree] =
        {
            left match {
                case h :: t => scan(t,
                    encode(h, maxSoFar < h.height) :: parsed,
                        Math.max(maxSoFar, h.height))
                case _ => parsed.reverse
            }
        }
        scan(trees, List(), -1)
    }

    def main(args: Array[String]): Unit =
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.toList)
            .filter(!_.isEmpty)
            .map(_.map(n => Tree(n.asDigit, false, false, false, false)))
            .map(tl => scanTreeLineFromLeft(tl,
                (t, v) => Tree(t.height, v, t.right, t.top, t.bottom)))
            .map(tl => scanTreeLineFromLeft(tl.reverse,
                (t, v) => Tree(t.height, t.left, v, t.top, t.bottom))
                .reverse)
            .transpose
            .map(tl => scanTreeLineFromLeft(tl,
                (t, v) => Tree(t.height, t.left, t.right, v, t.bottom)))
            .map(tl => scanTreeLineFromLeft(tl.reverse,
                (t, v) => Tree(t.height, t.left, t.right, t.top, v))
                .reverse)
            .transpose
            
            // .map(_.map(t => (t.height, 
            //     s"${if (t.left) 'T' else 'F'}" +
            //     s"${if (t.right) 'T' else 'F'}" +
            //     s"${if (t.top) 'T' else 'F'}" +
            //     s"${if (t.bottom) 'T' else 'F'}")))
            // .foreach(println)

            .foldLeft(0)((z, tl) => z + tl
                .foldLeft(0)((y, t) => y + (if (t.left || t.right || t.top || t.bottom) 1 else 0))))
    }
}