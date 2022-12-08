import scala.annotation.tailrec
import collection.immutable.Queue

object Puzzle2BetterIdea
{
    trait InElem
    {}
    case class File(name: String, size: Int) extends InElem
    {}
    case class ChangeDirectory(to: String) extends InElem
    {}

    def calculateDirSizes(data: List[InElem]): Map[String, Int] =
    {
        @tailrec
        def scanData(left: List[InElem], path: List[String],
            dirsizes: Map[String, Int]): Map[String, Int] =
        {
            left match {
                case File(name, size) :: t => scanData(t, path, updateSizeMap(size, dirsizes, path))
                case ChangeDirectory(to) :: t => to match {
                    case "/" => scanData(t, List("/"), dirsizes)
                    case ".." => scanData(t, path.tail, dirsizes)
                    case _ => scanData(t, to :: path, dirsizes)
                }
                case _ => dirsizes
            }
        }
        @tailrec
        def updateSizeMap(size: Int, map: Map[String, Int], dirs: List[String]): Map[String, Int] =
        {
            dirs match {
                case h :: t  => {
                    val absName = dirs.mkString("/")
                    if (map.contains(absName))
                        updateSizeMap(size, map + (absName -> (map(absName) + size)), t)
                    else
                        updateSizeMap(size, map + (absName -> size), t)
                }
                case _ => map
            }
        }

        scanData(data, List("/"), Map())
    }

    def main(args: Array[String]): Unit =
    {
        val cdPattern = "\\$ cd ([^\\s]+)".r
        val dirPattern = "dir (\\w+)".r
        val filePattern = "(\\d+) ([^\\s]+)".r

        val data = io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_ match {
                case cdPattern(to) => Some(ChangeDirectory(to))
                case filePattern(len, name) => Some(File(name, len.toInt))
                case _ => None
            })
            .filter(_.isDefined)
            .map(_.get)

        val dirSizes = calculateDirSizes(data)

        val totalMem =       70000000
        val requiredUnused = 30000000
        val usedMem = totalMem - dirSizes("/")
        val toFree = requiredUnused - usedMem

        println(dirSizes
            .toList
            .filter(t => t._2 > toFree)
            .minBy(t => t._2)
            ._2)
    }
}