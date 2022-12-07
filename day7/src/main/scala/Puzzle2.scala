import scala.annotation.tailrec
import collection.immutable.Queue

object Puzzle2
{
    trait InElem
    {}
    case class File(name: String, size: Int) extends InElem
    {}
    case class Directory(name: String) extends InElem
    {}
    case class ChangeDirectory(to: String) extends InElem
    {}

    def constructFileTree(data: List[InElem]): Map[String, List[InElem]] =
    {
        @tailrec
        def scanData(left: List[InElem], path: List[String],
            dircontent: List[(String, InElem)]): Map[String, List[InElem]] =
        {
            left match {
                case File(name, size) :: t => scanData(t, path, (path.mkString("/"), File(name, size)) :: dircontent)
                case Directory(name) :: t => scanData(t, path, (path.mkString("/"), Directory((name :: path).mkString("/"))) :: dircontent)
                case ChangeDirectory(to) :: t => to match {
                    case "/" => scanData(t, List("/"), dircontent)
                    case ".." => scanData(t, path.tail, dircontent)
                    case _ => scanData(t, to :: path, dircontent)
                }
                case _ => dircontent.groupBy(_._1).mapValues(_.map(_._2))
            }
        }

        scanData(data, List("/"), List())
    }

    def calculateDirectorySizes(fileTree: Map[String, List[InElem]]): Map[String, Int] =
    {
        @tailrec
        def calcSize(elems: List[InElem], sizes: Map[String, Int], totalSize: Int = 0): Option[Int] =
        {
            elems match {
                case h :: t => h match {
                    case File(name, size) => calcSize(t, sizes, totalSize + size)
                    case Directory(name) => if (sizes.contains(name)) 
                        calcSize(t, sizes, totalSize + sizes(name)) else None
                    case _ => calcSize(t, sizes, totalSize)
                }
                case _ => Some(totalSize)
            }
        }
        @tailrec
        def parseTree(dirs: Queue[String], sizes: Map[String, Int]): Map[String, Int] =
        {
            dirs match {
                case h +: t => {
                    calcSize(fileTree(h), sizes) match {
                        case Some(n) => parseTree(t, sizes + (h -> n))
                        case None => parseTree(t :+ h, sizes)
                    }
                }
                case _ => sizes
            }
        }
        parseTree(Queue(fileTree.keySet.toList: _*), Map())
    }

    // Ordinary recursion

    // def calculateDirectorySizes(fileTree: Map[String, List[InElem]]): Map[String, Int] =
    // {
    //     def calcSize(elems: List[InElem], sizes: Map[String, Int], totalSize: Int = 0): Int =
    //     {
    //         elems match {
    //             case h :: t => h match {
    //                 case File(name, size) => calcSize(t, sizes, totalSize + size)
    //                 case Directory(name) => if (sizes.contains(name)) 
    //                     calcSize(t, sizes, totalSize + sizes(name)) else 
    //                     calcSize(t, sizes, totalSize + calcSize(fileTree(name), sizes))
    //                 case _ => calcSize(t, sizes, totalSize)
    //             }
    //             case _ => totalSize
    //         }
    //     }
    //     @tailrec
    //     def parseTree(dirs: List[String], sizes: Map[String, Int]): Map[String, Int] =
    //     {
    //         dirs match {
    //             case h +: t => parseTree(t, sizes + (h -> calcSize(fileTree(h), sizes)))
    //             case _ => sizes
    //         }
    //     }

    //     parseTree(fileTree.keySet.toList, Map())
    // }

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
                case dirPattern(name) => Some(Directory(name))
                case filePattern(len, name) => Some(File(name, len.toInt))
                case _ => None
            })
            .filter(_.isDefined)
            .map(_.get)

        val fileTree = constructFileTree(data)
        val dirSizes = calculateDirectorySizes(fileTree)
        
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