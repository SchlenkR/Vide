
module LocSta.Core

open System

let printfn s = System.Diagnostics.Debug.WriteLine s

// TODO: struct tuples

type Gen<'v,'s,'r> = Gen of ('s option -> 'r -> ('v * 's))
type NoState = NoState

type Type with
    member this.DisplayName =
        let rec getTypeName (t: Type) =
            match t.GenericTypeArguments with
            | [| |] -> t.Name
            | args ->
                let args = args |> Array.map getTypeName |> String.concat ", "
                $"{t.Name}<{args}>"
        getTypeName this

module Gen =
    let inline run (Gen g) = g

    /// Bind with transparent (nested) state typing.
    let inline bind 
        (Gen m: Gen<'v1, 's1, 'r>)
        (f: 'v1 -> Gen<'v2, 's2, 'r>)
        : Gen<'v2, 's1 * 's2, 'r>
        =
        fun mfState r ->
            // unpack the previous state (may be None or Some)
            let mState,fState =
                match mfState with
                | None -> None,None
                | Some (mState,fState) -> Some mState, Some fState
            let mOut,mState' = m mState r
            let (Gen fgen) = f mOut
            let fOut,fState' = fgen fState r
            fOut, (mState', fState')
        |> Gen

    let inline ofValue x =
        Gen <| fun s r -> x,NoState

    let inline read<'r> : Gen<'r,_,'r> =
        Gen <| fun s r -> r,NoState

    let inline statesAndValue (Gen g) =
        Gen <| fun s r ->
            let v,s' = g s r
            {| inState = s; value = v; outState = s' |}, s'

    // TODO: Some of them should not be auto-opened
    let inline map proj (Gen g) = 
        Gen <| fun s r -> let o,s = g s r in proj o, s

    let inline preserve (factory: unit -> 's) = 
        Gen <| fun s r ->
            let state = s |> Option.defaultWith factory
            state,state

    let inline ofMutable<'v,'r> (initialValue: 'v) =
        Gen <| fun s (r: 'r) ->
            let refCell = s |> Option.defaultWith (fun () -> ref initialValue)
            let setter = fun value -> refCell.contents <- value
            (refCell.contents, setter), refCell

    let inline toEvaluable (Gen g: Gen<_,'s,_>) =
        let mutable state = None
        fun r ->
            let fOut,fState = g state r
            state <- Some fState
            fOut

    type GenBuilder() =
        member inline _.Bind(m, f) = bind m f
        member inline _.Return(x) = ofValue x
        member inline _.ReturnFrom(x) : Gen<_,_,_> = x

let gen = Gen.GenBuilder()
