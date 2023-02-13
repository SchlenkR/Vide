
open System
open System.Collections.Generic

type DateTime with
    member this.Show() = this.ToString()

type IEnumerable<'a> with
    member this.Show() =
        this
        |> Seq.tryHead
        |> Option.map (fun x -> x.ToString())
        |> Option.defaultValue "-"
        |> fun x -> x.ToString()

type Context<^a when ^a : (member Show: unit -> string)>(a: ^a) =
    member inline _.Process() = printfn $"Value is: {a.Show()}"

// Augmented members are are not yet visible to SRTP constraints
let enumerableContext = Context(seq {4})
