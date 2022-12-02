object Puzzle2
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
        //         case ("A", "X") => 0 + 3
        //         case ("A", "Y") => 3 + 1
        //         case ("A", "Z") => 6 + 2
        //         case ("B", "X") => 0 + 1
        //         case ("B", "Y") => 3 + 2
        //         case ("B", "Z") => 6 + 3
        //         case ("C", "X") => 0 + 2
        //         case ("C", "Y") => 3 + 3
        //         case ("C", "Z") => 6 + 1
        //     })
        //     .sum)

        //Lookup-based solution
        val shapeScores = Array(3, 1, 2, 3, 1)

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
                case ("A", myMov) => (0, myMov)
                case ("B", myMov) => (1, myMov)
                case ("C", myMov) => (2, myMov)
            })
            .map(_ match
            {
                case (oppMvInd, "X") => 0 + shapeScores(oppMvInd + 0)
                case (oppMvInd, "Y") => 3 + shapeScores(oppMvInd + 1)
                case (oppMvInd, "Z") => 6 + shapeScores(oppMvInd + 2)
            })
            .sum)
    }
}