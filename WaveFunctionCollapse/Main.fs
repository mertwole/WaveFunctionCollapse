module Main

open System.Collections.Generic

open Field
open Rules


type CollapsedField = {
    Field: Field
    CollapsePosition: int * int
    // TODO: newtype wrapper
    Attempts: char list
}

type CollapsedCell = {
    Position: int * int
    Value: char
}

type ApplyRulesResult = {
    NewField: Field
    changed: bool
}

let possibleValuesDueRules(adjacent: Superposition, adjacencyDirection: Direction, rules: Rule list): Superposition =     
    let variants = 
        rules
        |> Seq.filter(
            fun rule -> 
                rule.Adjacency = adjacencyDirection && (List.contains rule.AdjacentTile adjacent.Variants)
            )
        |> Seq.map(fun rule -> rule.MainTile)
        |> Seq.distinct
        |> Seq.toList
    
    Superposition(variants)

let tryApplyRulesToCell(field: Field, position: int * int, rules: Rule list): Superposition =
    let (cellX, cellY) = position

    let possibleValues =
        [Direction.Up; Direction.Down; Direction.Left; Direction.Right]
        |> Seq.choose (
            fun dir -> 
                let (offsetX, offsetY) = 
                    match dir with 
                        | Up -> (0, -1)
                        | Down -> (0, 1)
                        | Left -> (-1, 0)
                        | Right -> (1, 0)

                let (adjacentX, adjacentY) = (cellX + offsetX, cellY + offsetY)

                let outOfBounds = adjacentX < 0 || adjacentX >= field.Width() || adjacentY < 0 || adjacentY >= field.Height()

                if outOfBounds then
                    None
                else
                    let adjacent = field.GetCell(adjacentX, adjacentY)
                    Some (possibleValuesDueRules(adjacent, dir, rules))
            )
        |> Seq.reduce (
            // TODO: It will break when there're no available directions, which means we work on field 1x1
            fun acc x -> acc.Intersection(x)
        )

    possibleValues.Intersection(field.GetCell(position))

let tryApplyRules(field: Field, rules: Rule list): ApplyRulesResult option =
    let newField = field.Clone()

    let mutable changed = false
    let mutable contradiction = false

    for y in 0..field.Height() - 1 do
        for x in 0..field.Width() - 1 do
            let oldValue = field.GetCell(x, y)
            let newValue = tryApplyRulesToCell(field, (x, y), rules)

            newField.SetCell(x, y, newValue)

            if newValue.Variants.Length = 0 then
                contradiction <- true

            if newValue.Variants.Length <> oldValue.Variants.Length then
                changed <- true

    if contradiction then
        None
    else
        Some { NewField = newField; changed = changed }

let rec tryApplyRulesLoop(field: Field, rules: Rule list): Field option =
    match tryApplyRules(field, rules) with
    | Some result -> 
        if result.changed then
            tryApplyRulesLoop(result.NewField, rules)
        else
            Some result.NewField
    | None -> None

let tryCollapseCell(field: Field, cell: CollapsedCell, rules: Rule list): Field option =
    let (collapseX, collapseY) = cell.Position
    field.SetCell(collapseX, collapseY, Superposition([cell.Value]))
    tryApplyRulesLoop(field, rules)

let processCollapse(collapse: CollapsedField, rules: Rule list): Stack<CollapsedField> =
    let collapsedCell = collapse.Field.GetCell(collapse.CollapsePosition)

    // TODO: Make decision based on tile probabilities derived from input sample.
    let nextAttempt = 
        collapsedCell.Variants 
        |> Seq.tryFind (fun v -> (not (List.contains v collapse.Attempts)))

    let newCollapses = Stack()

    match nextAttempt with
        | Some attempt -> 
            newCollapses.Push({ collapse with Attempts = collapse.Attempts @ [attempt] })

            let (collapseX, collapseY) = collapse.CollapsePosition
            let newField = collapse.Field.Clone()
            let collapsedCell = { Position = collapse.CollapsePosition; Value = attempt  }

            match tryCollapseCell(newField, collapsedCell, rules) with
                | Some field -> 
                    // TODO: Select based on entropy metric
                    let (fieldWidth, _) = field.Size()
                    let nextCollapsePosition =
                        if collapseX = fieldWidth - 1 then
                            (0, collapseY + 1)
                        else 
                            (collapseX + 1, collapseY)

                    let newCollapse = { Field = field; CollapsePosition = nextCollapsePosition; Attempts = [] }
                    newCollapses.Push(newCollapse)
                | None -> ()
        | None -> ()

    newCollapses

let tryCollapse(initialField: Field, rules: Rule list) : Field option =
    let collapses = new Stack<CollapsedField>()
    let initialCollapse = { Field = initialField; CollapsePosition = (0, 0); Attempts = [] }
    
    collapses.Push(initialCollapse)

    let fieldArea = initialField.Width() * initialField.Height()

    while collapses.Count > 0 && collapses.Count <= fieldArea do
        let latest = collapses.Pop()

        for collapse in processCollapse(latest, rules) |> Seq.toList |> Seq.rev do
            collapses.Push(collapse)
    
    match collapses.Count with
        | 0 -> None
        | _ -> Some (collapses.Pop().Field)

let sample = array2D([
    ['┌'; '─'; '─'; '┐']
    ['│'; '┌'; '┐'; '│']
    ['│'; '└'; '┘'; '│']
    ['└'; '─'; '─'; '┘']
])

let rules = parseRulesFromSample(sample)

let initialSuperposition = Superposition(['─'; '│'; '┌'; '┐'; '└'; '┘'])
let initialField = Field(5, 5, initialSuperposition)

initialField.SetCell(0, 0, Superposition(['└']))

let field = tryCollapse(initialField, rules)

match field with
    | Some field -> field.Print()
    | None -> printfn "Failed to collapse field."
