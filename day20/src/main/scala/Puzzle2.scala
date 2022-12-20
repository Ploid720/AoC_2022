import scala.annotation.tailrec

object Puzzle2
{
    def mod(x: Long, y: Long) = x % y + (if (x < 0) y else 0)

    @tailrec
    def mix(data: Map[(Long, Long), Long], order: List[(Long, Long)], 
        passes: Int): Map[(Long, Long), Long] =
    {
        if (passes <= 0)
            data
        else
        {
            val totalCount = order.size

            val map = order.zipWithIndex
                .map(t => (t._1, t._2.toLong))
                .foldLeft(data)((z, a) => {
                    val baseMove = a._1._1
                    val move = baseMove % (totalCount - 1)

                    val oldInd = z.getOrElse(a._1, a._2)
                    val newIndRaw = oldInd + move
                    val newInd = mod(newIndRaw, totalCount)

                    if (oldInd < newInd)
                    {
                        if (newIndRaw == newInd)
                        {
                            if (newInd == (totalCount - 1))
                                z.map(t => (t._1, if (t._2 < oldInd) 
                                    (t._2 + 1) else t._2)) + (a._1 -> 0)
                            else
                                z.map(t => (t._1, if ((t._2 > oldInd) && (t._2 <= newInd)) 
                                    (t._2 - 1) else t._2)) + (a._1 -> newInd)
                        }
                        else
                            z.map(t => (t._1, if ((t._2 > oldInd) && (t._2 < newInd)) 
                                (t._2 - 1) else t._2)) + (a._1 -> mod(newInd - 1, totalCount))
                    }
                    else if (oldInd > newInd)
                    {
                        if (newIndRaw == newInd)
                        {
                            if (newInd == 0)
                                z.map(t => (t._1, if (t._2 > oldInd) 
                                    (t._2 - 1) else t._2)) + (a._1 -> (totalCount - 1))
                            else
                                z.map(t => (t._1, if ((t._2 < oldInd) && (t._2 >= newInd)) 
                                    (t._2 + 1) else t._2)) + (a._1 -> newInd)
                        } 
                        else
                            z.map(t => (t._1, if ((t._2 < oldInd) && (t._2 > newInd)) 
                                (t._2 + 1) else t._2)) + (a._1 -> mod(newInd + 1, totalCount))
                    }
                    else
                        z
                    })

            mix(map, order, passes - 1)
        }
    }

    def main(args: Array[String]): Unit =
    {
        val key = 811589153

        val nums = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.toLong * 811589153)
            .zipWithIndex
            .map(t => (t._1, t._2.toLong))
        val totalCount = nums.size

        totalCount.ensuring(_ == nums.toSet.size)

        val map = mix(nums.zipWithIndex.map(t => (t._1, t._2.toLong)).toMap, nums, 10)

        val zeroPos = map.find(_._1._1 == 0).get._2
        val v1000ind = mod(zeroPos + 1000, totalCount)
        val v2000ind = mod(zeroPos + 2000, totalCount)
        val v3000ind = mod(zeroPos + 3000, totalCount)
        val v1000 = map.find(_._2 == v1000ind).get._1._1
        val v2000 = map.find(_._2 == v2000ind).get._1._1
        val v3000 = map.find(_._2 == v3000ind).get._1._1

        println(v1000 + v2000 + v3000)
    }
}