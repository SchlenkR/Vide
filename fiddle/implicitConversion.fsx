
module A =
    type Vide<'v,'s,'c>() =
        static member op_Implicit(source: Vide<_,_,_>) : int =
            1

    let doIt (x: int) = x

    doIt (Vide())


type Vide<'v,'s,'c>(value: int) =
    member _.value = value
    static member op_Explicit(source: Vide<_,_,_>) : (unit -> Vide<_,_,_>) =
        fun () -> source

let doIt (v: unit -> Vide<_,_,_>) = v()

doIt (Vide 1 :> (unit -> Vide<_,_,_>))

