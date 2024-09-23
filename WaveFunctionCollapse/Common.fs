module Common

type Vector2 = 
    { X: int; Y: int }

    static member (+) (lhs: Vector2, rhs: Vector2): Vector2 =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y }

    static member (-) (lhs: Vector2, rhs: Vector2): Vector2 =
        { X = lhs.X - rhs.X; Y = lhs.Y - rhs.Y }

    static member Zero: Vector2 = { X = 0; Y = 0 }

type Direction = 
    Up | Down | Left | Right

    member this.ToVector2(): Vector2 = 
        match this with 
        | Up -> { X = 0; Y = -1 }
        | Down -> { X = 0; Y = 1 }
        | Left -> { X = -1; Y = 0 }
        | Right -> { X = 1; Y = 0 }

    member this.Negate: Direction = 
        match this with
        | Up -> Down
        | Down -> Up
        | Left -> Right
        | Right -> Left
