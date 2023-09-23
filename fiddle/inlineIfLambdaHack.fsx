
// Look at the decompiled C# in sharpLab,
// play with the InlineIfLambdaAttribute in module IILShadowing,
// search for "finalResult" in the decompiled C# code.

type Vide = string -> string

[<AutoOpen>]
module VideFunction =
    let inline mkVide ([<InlineIfLambda>] v: Vide) = v
    let inline runVide ([<InlineIfLambda>] v: Vide) = v

module IILShadowing =
    // type InlineIfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute
    type InlineIfLambdaAttribute() = inherit System.Attribute()

let inline bind 
    ([<InlineIfLambda>] f: string -> Vide)
    ([<IILShadowing.InlineIfLambdaAttribute>] m: Vide) 
    =
    mkVide <| fun x ->
        let mres = runVide m x
        let fvide = f mres
        runVide fvide x

let ret v = mkVide <| fun _ -> v

type Builder() =
    member inline _.Bind([<IILShadowing.InlineIfLambdaAttribute>] m, [<InlineIfLambda>] f) =
        bind f m
    member _.Return(x) = ret x

let b = Builder()

// ...

let show = mkVide <| fun x -> x.ToString()
let toList = mkVide <| fun x -> $"[{x}]"

let comp = b {
    let! x = show
    let! xInList = toList
    return $"The value '{x}' wrapped in a list looks like '{xInList}'."
}

let finalRes = comp "whateverValue"

printfn "%s" finalRes
