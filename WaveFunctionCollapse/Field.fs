module Field

open Common

type Superposition(vars: char list) =
    let variants = vars

    member this.Variants
        with get () = variants

    member this.Intersection(other: Superposition): Superposition =
        let intersection = 
            variants
            |> Seq.filter (fun var -> List.contains var other.Variants)
            |> Seq.toList
        Superposition(intersection)
        
    member this.GetChar() =
        match variants.Length with
            | 1 -> variants.[0]
            | 0 -> ' '
            | _ -> '?'

    member this.ToDebugString(lengthPadding: int): string =
        let str = new string(List.toArray variants)
        let str = str.PadRight(lengthPadding)
        $"[{str}]"

type Field private (field_: Superposition array2d) = 
    let field = field_

    new(size: Vector2, initialValues: Superposition) = 
        Field(Array2D.create size.Y size.X initialValues)

    member this.Size(): Vector2 = { X = field.GetLength(1); Y = field.GetLength(0) }

    member this.Width(): int = field.GetLength(1)

    member this.Height(): int = field.GetLength(0)

    member this.Clone(): Field = 
        let size = this.Size()
        let newField = Array2D.create size.Y size.X (Superposition([]))
        for y in 0..this.Height() - 1 do
            for x in 0..this.Width() - 1 do
                newField.[y, x] <- field.[y, x]
        Field(newField)

    member this.GetCell(position: Vector2): Superposition = 
        field.[position.Y, position.X]

    member this.SetCell(position: Vector2, tile: Superposition) = 
        field.[position.Y, position.X] <- tile

    member this.Print() = 
        for y in 0..this.Height() - 1 do
            for x in 0..this.Width() - 1 do
                printf "%c" (field.[y, x].GetChar())
            printfn ""

    member this.PrintDebug() = 
        let maxSuperpositionSize = 
            field
            |> Seq.cast<Superposition>
            |> Seq.map (fun s -> s.Variants.Length)
            |> Seq.max
        
        for y in 0..this.Height() - 1 do
            for x in 0..this.Width() - 1 do
                printf "%s" (field.[y, x].ToDebugString(maxSuperpositionSize))
            printfn ""
