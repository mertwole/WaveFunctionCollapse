module Field

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

    new(width: int, height: int, initialValues: Superposition) = 
        Field(Array2D.create width height initialValues)

    member this.Size(): (int * int) = (field.GetLength(0), field.GetLength(1))

    member this.Width(): int = field.GetLength(0)

    member this.Height(): int = field.GetLength(1)

    member this.Clone(): Field = 
        let (width, height) = this.Size()
        let newField = Array2D.create width height (Superposition([]))
        for y in 0..field.GetLength(1) - 1 do
            for x in 0..field.GetLength(0) - 1 do
                newField.[x, y] <- field.[x, y]
        Field(newField)

    // TODO: newtype wrapper for coordinates
    member this.GetCell(x: int, y: int): Superposition = 
        field.[x, y]

    member this.SetCell(x: int, y: int, tile: Superposition) = 
        field.[x, y] <- tile

    member this.Print() = 
        for y in 0..field.GetLength(1) - 1 do
            for x in 0..field.GetLength(0) - 1 do
                printf "%c" (field.[x, y].GetChar())
            printfn ""

    member this.PrintDebug() = 
        let maxSuperpositionSize = 
            field
            |> Seq.cast<Superposition>
            |> Seq.map (fun s -> s.Variants.Length)
            |> Seq.max
        
        for y in 0..field.GetLength(1) - 1 do
            for x in 0..field.GetLength(0) - 1 do
                printf "%s" (field.[x, y].ToDebugString(maxSuperpositionSize))
            printfn ""
