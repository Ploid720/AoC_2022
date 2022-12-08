import scala.annotation.tailrec

object Puzzle2
{
    def scoreTree(height: Int, x: Int, y: Int, treeLookup: Array[Array[Int]]): Int =
    {
        val gridWidth = treeLookup(0).size
        val gridHeight = treeLookup.size

        @tailrec
        def scanLeft(currX: Int, count: Int = 0): Int = 
        {
            if (currX < 0)
                count
            else if (treeLookup(y)(currX) >= height)
                count + 1
            else
                scanLeft(currX - 1, count + 1)
        }
        @tailrec
        def scanRight(currX: Int, count: Int = 0): Int = 
        {
            if (currX >= gridWidth)
                count
            else if (treeLookup(y)(currX) >= height)
                count + 1
            else
                scanRight(currX + 1, count + 1)
        }
        @tailrec
        def scanUp(currY: Int, count: Int = 0): Int = 
        {
            if (currY < 0)
                count
            else if (treeLookup(currY)(x) >= height)
                count + 1
            else
                scanUp(currY - 1, count + 1)
        }
        @tailrec
        def scanDown(currY: Int, count: Int = 0): Int = 
        {
            if (currY >= gridHeight)
                count
            else if (treeLookup(currY)(x) >= height)
                count + 1
            else
                scanDown(currY + 1, count + 1)
        }

        scanLeft(x - 1) * scanRight(x + 1) * scanUp(y - 1) * scanDown(y + 1)
    }

    def main(args: Array[String]): Unit =
    {
        val treeList = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.toList)
            .filter(!_.isEmpty)
            .map(_.map(_.asDigit))
        val treeLookup = treeList
            .map(_.toArray)
            .toArray
        
        println(treeList
            .map(_.zipWithIndex)
            .zipWithIndex
            .flatMap(tl => tl._1.map(t => scoreTree(t._1, t._2, tl._2, treeLookup)))
            .max)
    }
}