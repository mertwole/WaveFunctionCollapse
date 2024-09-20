module Rules

type Direction = Up | Down | Left | Right

let private directionToOffset(dir: Direction): (int * int) = 
    match dir with 
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

type Rule = {
    MainTile: char
    AdjacentTile: char
    Adjacency: Direction
}

let parseRulesFromSample(sample: char[,]): Rule list =
    seq { 0 .. sample.GetLength(0) - 1 }
    |> Seq.allPairs (seq { 0 .. sample.GetLength(1) - 1 })
    |> Seq.allPairs (Seq.ofList [Direction.Up; Direction.Down; Direction.Left; Direction.Right])
    |> Seq.choose (fun (dir, (y, x)) -> 
            let (offsetX, offsetY) = directionToOffset dir
            let (adjacentX, adjacentY) = (x + offsetX, y + offsetY)
                
            if adjacentX < 0 
                || adjacentX >= sample.GetLength(0) 
                || adjacentY < 0 
                || adjacentY >= sample.GetLength(1) 
            then
                None
            else
                let main = sample.[y, x]
                let adjacent = sample.[adjacentY, adjacentX]
                Some { MainTile = main; AdjacentTile = adjacent; Adjacency = dir }
        )
    |> Seq.distinct
    |> Seq.toList
