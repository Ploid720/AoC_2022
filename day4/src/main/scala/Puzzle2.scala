object Puzzle2
{
    def main(args: Array[String])
    {
        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(_.split(",") match
            {
                case Array(a, b) => List(a, b)
                case _ => throw new IllegalArgumentException
            })
            .map(_.map(_.split("-") match
            {
                case Array(a, b) => (a.toInt, b.toInt)
                case _ => throw new IllegalArgumentException
            }))
            .filter(_ match
            {
                case List((al, au), (bl, bu))
                    if ((au >= bl) && (al <= bu)) => true
                case List((al, au), (bl, bu))
                    if ((bu >= al) && (bl <= au)) => true
                case _ => false
            })
            .size)
    }
}