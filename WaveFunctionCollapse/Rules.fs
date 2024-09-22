module Rules

open Common

type Rule = {
    MainTile: char
    AdjacentTile: char
    Adjacency: Direction
}

type RuleSet = {
    Rules: Rule list
}

let parseRulesFromSample(sample: char[,]): RuleSet =
    let rules = 
        seq { 0 .. sample.GetLength(0) - 1 }
        |> Seq.allPairs (seq { 0 .. sample.GetLength(1) - 1 })
        |> Seq.allPairs (Seq.ofList [Direction.Up; Direction.Down; Direction.Left; Direction.Right])
        |> Seq.choose (fun (dir, (x, y)) -> 
                let position = { X = x; Y = y }
                let adjacent = position + dir.ToVector2()

                if adjacent.X < 0 
                    || adjacent.X >= sample.GetLength(1) 
                    || adjacent.Y < 0 
                    || adjacent.Y >= sample.GetLength(0) 
                then
                    None
                else
                    let main = sample.[y, x]
                    let adjacent = sample.[adjacent.Y, adjacent.X]
                    Some { MainTile = main; AdjacentTile = adjacent; Adjacency = dir }
            )
        |> Seq.distinct
        |> Seq.toList

    { Rules = rules }
