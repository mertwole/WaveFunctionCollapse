module Rules

type Direction = Up | Down | Left | Right

type Rule = {
    MainTile: char
    AdjacentTile: char
    Adjacency: Direction
}

let parseRulesFromSample(sample: char[,]): Rule list =
    let rules = [
        { MainTile = '─'; AdjacentTile = '┘'; Adjacency = Up }
        { MainTile = '─'; AdjacentTile = '└'; Adjacency = Up }
        { MainTile = '─'; AdjacentTile = '─'; Adjacency = Up }

        { MainTile = '─'; AdjacentTile = '┐'; Adjacency = Down }
        { MainTile = '─'; AdjacentTile = '┌'; Adjacency = Down }
        { MainTile = '─'; AdjacentTile = '─'; Adjacency = Down }

        { MainTile = '─'; AdjacentTile = '─'; Adjacency = Left }
        { MainTile = '─'; AdjacentTile = '┌'; Adjacency = Left }
        { MainTile = '─'; AdjacentTile = '└'; Adjacency = Left }

        { MainTile = '─'; AdjacentTile = '─'; Adjacency = Right }
        { MainTile = '─'; AdjacentTile = '┐'; Adjacency = Right }
        { MainTile = '─'; AdjacentTile = '┘'; Adjacency = Right }

    
        { MainTile = '│'; AdjacentTile = '│'; Adjacency = Up }
        { MainTile = '│'; AdjacentTile = '┌'; Adjacency = Up }
        { MainTile = '│'; AdjacentTile = '┐'; Adjacency = Up }

        { MainTile = '│'; AdjacentTile = '│'; Adjacency = Down }
        { MainTile = '│'; AdjacentTile = '└'; Adjacency = Down }
        { MainTile = '│'; AdjacentTile = '┘'; Adjacency = Down }

        { MainTile = '│'; AdjacentTile = '┘'; Adjacency = Left }
        { MainTile = '│'; AdjacentTile = '┐'; Adjacency = Left }
        { MainTile = '│'; AdjacentTile = '│'; Adjacency = Left }

        { MainTile = '│'; AdjacentTile = '└'; Adjacency = Right }
        { MainTile = '│'; AdjacentTile = '┌'; Adjacency = Right }
        { MainTile = '│'; AdjacentTile = '│'; Adjacency = Right }


        { MainTile = '┌'; AdjacentTile = '┘'; Adjacency = Up }
        { MainTile = '┌'; AdjacentTile = '└'; Adjacency = Up }
        { MainTile = '┌'; AdjacentTile = '─'; Adjacency = Up }

        { MainTile = '┌'; AdjacentTile = '│'; Adjacency = Down }
        { MainTile = '┌'; AdjacentTile = '└'; Adjacency = Down }
        { MainTile = '┌'; AdjacentTile = '┘'; Adjacency = Down }

        { MainTile = '┌'; AdjacentTile = '┘'; Adjacency = Left }
        { MainTile = '┌'; AdjacentTile = '┐'; Adjacency = Left }
        { MainTile = '┌'; AdjacentTile = '│'; Adjacency = Left }

        { MainTile = '┌'; AdjacentTile = '─'; Adjacency = Right }
        { MainTile = '┌'; AdjacentTile = '┐'; Adjacency = Right }
        { MainTile = '┌'; AdjacentTile = '┘'; Adjacency = Right }


        { MainTile = '┐'; AdjacentTile = '┘'; Adjacency = Up }
        { MainTile = '┐'; AdjacentTile = '└'; Adjacency = Up }
        { MainTile = '┐'; AdjacentTile = '─'; Adjacency = Up }

        { MainTile = '┐'; AdjacentTile = '│'; Adjacency = Down }
        { MainTile = '┐'; AdjacentTile = '└'; Adjacency = Down }
        { MainTile = '┐'; AdjacentTile = '┘'; Adjacency = Down }

        { MainTile = '┐'; AdjacentTile = '─'; Adjacency = Left }
        { MainTile = '┐'; AdjacentTile = '┌'; Adjacency = Left }
        { MainTile = '┐'; AdjacentTile = '└'; Adjacency = Left }

        { MainTile = '┐'; AdjacentTile = '└'; Adjacency = Right }
        { MainTile = '┐'; AdjacentTile = '┌'; Adjacency = Right }
        { MainTile = '┐'; AdjacentTile = '│'; Adjacency = Right }


        { MainTile = '└'; AdjacentTile = '│'; Adjacency = Up }
        { MainTile = '└'; AdjacentTile = '┌'; Adjacency = Up }
        { MainTile = '└'; AdjacentTile = '┐'; Adjacency = Up }

        { MainTile = '└'; AdjacentTile = '┐'; Adjacency = Down }
        { MainTile = '└'; AdjacentTile = '┌'; Adjacency = Down }
        { MainTile = '└'; AdjacentTile = '─'; Adjacency = Down }

        { MainTile = '└'; AdjacentTile = '┘'; Adjacency = Left }
        { MainTile = '└'; AdjacentTile = '┐'; Adjacency = Left }
        { MainTile = '└'; AdjacentTile = '│'; Adjacency = Left }

        { MainTile = '└'; AdjacentTile = '─'; Adjacency = Right }
        { MainTile = '└'; AdjacentTile = '┐'; Adjacency = Right }
        { MainTile = '└'; AdjacentTile = '┘'; Adjacency = Right }


        { MainTile = '┘'; AdjacentTile = '│'; Adjacency = Up }
        { MainTile = '┘'; AdjacentTile = '┌'; Adjacency = Up }
        { MainTile = '┘'; AdjacentTile = '┐'; Adjacency = Up }

        { MainTile = '┘'; AdjacentTile = '┐'; Adjacency = Down }
        { MainTile = '┘'; AdjacentTile = '┌'; Adjacency = Down }
        { MainTile = '┘'; AdjacentTile = '─'; Adjacency = Down }

        { MainTile = '┘'; AdjacentTile = '─'; Adjacency = Left }
        { MainTile = '┘'; AdjacentTile = '┌'; Adjacency = Left }
        { MainTile = '┘'; AdjacentTile = '└'; Adjacency = Left }

        { MainTile = '┘'; AdjacentTile = '└'; Adjacency = Right }
        { MainTile = '┘'; AdjacentTile = '┌'; Adjacency = Right }
        { MainTile = '┘'; AdjacentTile = '│'; Adjacency = Right }
    ]

    rules
