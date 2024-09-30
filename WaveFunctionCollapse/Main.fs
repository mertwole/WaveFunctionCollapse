module Main

open System.Collections.Generic

open Common
open Field
open Rules

type CollapsedField = {
    Field: Field
    CollapsePosition: Vector2
    Attempts: char list
}

type CollapsedCell = {
    Position: Vector2
    Value: char
}

type ApplyRulesResult = {
    NewField: Field
    Changed: bool
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

let determineNextPosition(fieldSize: Vector2, position: Vector2): Vector2 option =
    if position.X = fieldSize.X - 1 then
        if position.Y = fieldSize.Y - 1 then 
            None
        else
            Some { X = 0; Y = position.Y + 1 }
    else
        Some { position with X = position.X + 1 }
    
let rec tryApplyRulesInner(field: Field, position: Vector2, rules: RuleSet): ApplyRulesResult option =
    let newField = field.Clone()
    let oldValue = field.GetCell(position)
    let newValue = tryApplyRulesToCell(field, position, rules)
    
    if newValue.Variants.Length = 0 then
        None
    else
        newField.SetCell(position, newValue)

        let changed = newValue.Variants.Length <> oldValue.Variants.Length

        match determineNextPosition(field.Size(), position) with
            | None -> 
                Some { NewField = newField; Changed = changed }
            | Some nextPosition -> 
                Option.map (
                        fun result -> { result with Changed = result.Changed || changed }
                    )
                    (tryApplyRulesInner(newField, nextPosition, rules))

let tryApplyRules(field: Field, rules: RuleSet): ApplyRulesResult option =
    tryApplyRulesInner(field, Vector2.Zero, rules)

let rec tryApplyRulesLoop(field: Field, rules: RuleSet): Field option =
    match tryApplyRules(field, rules) with
    | Some result -> 
        if result.Changed then
            tryApplyRulesLoop(result.NewField, rules)
        else
            Some result.NewField
    | None -> None

let tryCollapseCell(field: Field, cell: CollapsedCell, rules: RuleSet): Field option =
    field.SetCell(cell.Position, Superposition([cell.Value]))
    tryApplyRulesLoop(field, rules)

let determineMinimalEntropyCell(field: Field): Vector2 option =
    let positions = 
        seq { 0..field.Width() - 1 }
        |> Seq.allPairs (seq { 0..field.Height() - 1 })
        |> Seq.map (fun (y, x) -> { X = x; Y = y })
        |> Seq.filter (fun position -> field.GetCell(position).Variants.Length > 1)

    if Seq.isEmpty positions then
        None
    else
        Some (
            positions 
                |> Seq.randomShuffle // This makes results more interesting.
                |> Seq.minBy (fun position -> field.GetCell(position).Variants.Length)
        )

let rec weightedSelectInner(
    accumulator: float, 
    random: float, 
    weightedVariants: (char * float) list
): char =
    let (_, weight) = List.head weightedVariants
    if accumulator + weight >= random || weightedVariants.Length = 1 then
        let (tile, _) = weightedVariants.Head
        tile
    else
        weightedSelectInner(accumulator + weight, random, weightedVariants.Tail)

let weightedSelect(rules: RuleSet, variants: char list): char = 
    let weightedVariants = 
        variants
        |> Seq.choose (fun variant -> 
            rules.Weights.TryFind variant
            |> Option.map (fun weight -> (variant, weight))
        )
        |> Seq.toList

    let weightSum = 
        weightedVariants
        |> Seq.sumBy (fun (_, weight) -> weight)

    let rand = System.Random().NextDouble() * weightSum

    weightedSelectInner(0, rand, weightedVariants)

let processCollapse(collapse: CollapsedField, rules: RuleSet): Stack<CollapsedField> =
    let collapsedCell = collapse.Field.GetCell(collapse.CollapsePosition)

    let validValues = 
        collapsedCell.Variants 
        |> Seq.filter (fun v -> (not (collapse.Attempts |> List.contains v)))
        |> Seq.toList
    
    let nextAttempt = 
        match validValues with 
        | [] -> None
        | [single] -> Some single
        | multiple -> Some (weightedSelect(rules, multiple))
    
    let newCollapses = Stack()

    match nextAttempt with
        | Some attempt -> 
            newCollapses.Push({ collapse with Attempts = collapse.Attempts @ [attempt] })

            let newField = collapse.Field.Clone()
            let collapsedCell = { Position = collapse.CollapsePosition; Value = attempt  }

            match tryCollapseCell(newField, collapsedCell, rules) with
                | Some field ->
                    let nextCollapsePosition = determineMinimalEntropyCell(field)
                    
                    match nextCollapsePosition with
                    | Some nextCollapsePosition ->
                        let newCollapse = { Field = field; CollapsePosition = nextCollapsePosition; Attempts = [] }
                        newCollapses.Push(newCollapse)
                    | None ->
                        // TODO: early return here
                        let newCollapse = { Field = field; CollapsePosition = Vector2.Zero; Attempts = [] }
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
    [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
    [' '; '┌'; '─'; '─'; '┐'; '┌'; '┐'; ' ']
    [' '; '│'; '┌'; '┐'; '└'; '┘'; '│'; ' ']
    [' '; '│'; '└'; '┘'; '┌'; '┐'; '│'; ' ']
    [' '; '└'; '─'; '─'; '┘'; '└'; '┘'; ' ']
    [' '; ' '; ' '; ' '; ' '; ' '; ' '; ' ']
])

let rules = parseRulesFromSample(sample)

let initialSuperposition = Superposition(['─'; '│'; '┌'; '┐'; '└'; '┘'; ' '])
let initialField = Field({ X = 40; Y = 20 }, initialSuperposition)

initialField.SetCell({ X = 0; Y = 0 }, Superposition(['┌']))

let field = tryCollapse(initialField, rules)

match field with
    | Some field -> field.Print()
    | None -> printfn "Failed to collapse field."
