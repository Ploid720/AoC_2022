object Puzzle1
{
    def main(args: Array[String])
    {
        //Basic iteration through each case
        // println(io.Source
        //     .fromResource("input.txt")
        //     .getLines
        //     .toList
        //     .map(s => s.trim)
        //     .filter(s => !s.isEmpty)
        //     .map(s => s.split(" ") match
        //     {
        //         case Array(oppMov, myMov) => (oppMov, myMov)
        //         case _ => throw new IllegalArgumentException
        //     })
        //     .map(_ match
        //     {
        //         case ("A", "X") => 3 + 1
        //         case ("A", "Y") => 6 + 2
        //         case ("A", "Z") => 0 + 3
        //         case ("B", "X") => 0 + 1 
        //         case ("B", "Y") => 3 + 2
        //         case ("B", "Z") => 6 + 3
        //         case ("C", "X") => 6 + 1
        //         case ("C", "Y") => 0 + 2
        //         case ("C", "Z") => 3 + 3
        //     })
        //     .sum)

        //Lookup-based solution
        val matchScores = Array(0, 3, 6, 0, 3)

        println(io.Source
            .fromResource("input.txt")
            .getLines
            .toList
            .map(s => s.trim)
            .filter(s => !s.isEmpty)
            .map(s => s.split(" ") match
            {
                case Array(oppMov, myMov) => (oppMov, myMov)
                case _ => throw new IllegalArgumentException
            })
            .map(_ match
            {
                case ("A", myMov) => (1, myMov)
                case ("B", myMov) => (0, myMov)
                case ("C", myMov) => (2, myMov)
            })
            .map(_ match
            {
                case (oppMvInd, "X") => 1 + matchScores(oppMvInd + 0)
                case (oppMvInd, "Y") => 2 + matchScores(oppMvInd + 1)
                case (oppMvInd, "Z") => 3 + matchScores(oppMvInd + 2)
            })
            .sum)
    }
}