module Main

open System.Collections.Generic

open Common
open Field
open Rules

type CollapsedField = {
    Field: Field
    CollapsePosition: Vector2
    // TODO: newtype wrapper
    Attempts: char list
}

type CollapsedCell = {
    Position: Vector2
    Value: char
}

type ApplyRulesResult = {
    NewField: Field
    changed: bool
}

let possibleValuesDueRules(adjacent: Superposition, adjacencyDirection: Direction, rules: RuleSet): Superposition =     
    let variants = 
        rules.Rules
        |> Seq.filter(
            fun rule -> 
                rule.Adjacency = adjacencyDirection && (List.contains rule.AdjacentTile adjacent.Variants)
            )
        |> Seq.map(fun rule -> rule.MainTile)
        |> Seq.distinct
        |> Seq.toList
    
    Superposition(variants)

let tryApplyRulesToCell(field: Field, position: Vector2, rules: RuleSet): Superposition =
    let possibleValues =
        [Direction.Up; Direction.Down; Direction.Left; Direction.Right]
        |> Seq.choose (
            fun dir -> 
                let offset = dir.ToVector2()
                let adjacent = position + offset

                let outOfBounds = adjacent.X < 0 || adjacent.X >= field.Width() || adjacent.Y < 0 || adjacent.Y >= field.Height()

                if outOfBounds then
                    None
                else
                    let adjacent = field.GetCell(adjacent)
                    Some (possibleValuesDueRules(adjacent, dir, rules))
            )
        |> Seq.reduce (
            // TODO: It will break when there're no available directions, which means we work on field 1x1
            fun acc x -> acc.Intersection(x)
        )

    possibleValues.Intersection(field.GetCell(position))

let tryApplyRules(field: Field, rules: RuleSet): ApplyRulesResult option =
    let newField = field.Clone()

    let mutable changed = false
    let mutable contradiction = false

    for y in 0..field.Height() - 1 do
        for x in 0..field.Width() - 1 do
            let position = { X = x; Y = y }
            let oldValue = field.GetCell(position)
            let newValue = tryApplyRulesToCell(field, position, rules)

            newField.SetCell(position, newValue)

            if newValue.Variants.Length = 0 then
                contradiction <- true

            if newValue.Variants.Length <> oldValue.Variants.Length then
                changed <- true

    if contradiction then
        None
    else
        Some { NewField = newField; changed = changed }

let rec tryApplyRulesLoop(field: Field, rules: RuleSet): Field option =
    match tryApplyRules(field, rules) with
    | Some result -> 
        if result.changed then
            tryApplyRulesLoop(result.NewField, rules)
        else
            Some result.NewField
    | None -> None

let tryCollapseCell(field: Field, cell: CollapsedCell, rules: RuleSet): Field option =
    field.SetCell(cell.Position, Superposition([cell.Value]))
    tryApplyRulesLoop(field, rules)

let processCollapse(collapse: CollapsedField, rules: RuleSet): Stack<CollapsedField> =
    let collapsedCell = collapse.Field.GetCell(collapse.CollapsePosition)

    // TODO: Make decision based on tile probabilities derived from input sample.
    let nextAttempt = 
        collapsedCell.Variants 
        |> Seq.tryFind (fun v -> (not (List.contains v collapse.Attempts)))

    let newCollapses = Stack()

    match nextAttempt with
        | Some attempt -> 
            newCollapses.Push({ collapse with Attempts = collapse.Attempts @ [attempt] })

            let { X = collapseX; Y = collapseY } = collapse.CollapsePosition
            let newField = collapse.Field.Clone()
            let collapsedCell = { Position = collapse.CollapsePosition; Value = attempt  }

            match tryCollapseCell(newField, collapsedCell, rules) with
                | Some field -> 
                    // TODO: Select based on entropy metric
                    let nextCollapsePosition =
                        if collapseX = field.Width() - 1 then
                            { X = 0; Y = collapseY + 1 }
                        else 
                            { X = collapseX + 1; Y = collapseY}

                    let newCollapse = { Field = field; CollapsePosition = nextCollapsePosition; Attempts = [] }
                    newCollapses.Push(newCollapse)
                | None -> ()
        | None -> ()

    newCollapses

let tryCollapse(initialField: Field, rules: RuleSet) : Field option =
    let collapses = new Stack<CollapsedField>()
    let initialCollapse = { Field = initialField; CollapsePosition = Vector2.Zero; Attempts = [] }
    
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
let initialField = Field({ X = 5; Y = 5 }, initialSuperposition)

initialField.SetCell({ X = 0; Y = 0 }, Superposition(['└']))

let field = tryCollapse(initialField, rules)

match field with
    | Some field -> field.Print()
    | None -> printfn "Failed to collapse field."
