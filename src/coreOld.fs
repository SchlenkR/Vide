
module LocSta.Core

open System
open Fable.Core

let printfn s = System.Diagnostics.Debug.WriteLine s

[<Emit("$0 === undefined")>]
let isUndefined (x: 'a) : bool = jsNative

type Type with
    member this.DisplayName =
        let rec getTypeName (t: Type) =
            match t.GenericTypeArguments with
            | [| |] -> t.Name
            | args ->
                let args = args |> Array.map getTypeName |> String.concat ", "
                $"{t.Name}<{args}>"
        getTypeName this

module TypeInfo =
    type ITypeInfo =
        abstract member runtimeType: Type with get,set

    let inline set (t: Type) (x: 'a) =
        let hti = box x :?> ITypeInfo
        if isUndefined hti.runtimeType |> not then
            failwith $"TypeInfo for type {t} is already set to: {hti.runtimeType.DisplayName}"
        hti.runtimeType <- t

    let inline get (x: 'a) =
        let hti = box x :?> ITypeInfo
        let ti = hti.runtimeType
        if isUndefined ti then
            failwith $"TypeInfo was undefined for type: {typeof<'a>.DisplayName}"
        ti

    let inline cast<'a> (expectedType: Type) (x: obj) =
        let runtimeType = get x
        if runtimeType <> expectedType then
            failwith $"cannot cast instance of type '{runtimeType.DisplayName}' to '{expectedType.DisplayName}'."
        x :?> 'a

// TODO: struct tuples

type Gen<'v,'s,'r> = Gen of ('s option -> 'r -> ('v * 's))
type NoState = NoState

let nostate =
    let x = NoState
    do TypeInfo.set typeof<NoState> x
    x

module Gen =
    let inline run (Gen g) = g

    /// Bind with transparent (nested) state typing.
    let inline bind 
        (Gen m: Gen<'v1, 's1, 'r>)
        (f: 'v1 -> Gen<'v2, 's2, 'r>)
        : Gen<'v2, 's1 * 's2, 'r>
        =
        fun mfState r ->
            let mState,fState =
                match mfState with
                | None -> None,None
                | Some (mState,fState) -> Some mState, Some fState
            let mOut,mState' = m mState r
            let (Gen fgen) = f mOut
            let fOut,fState' = fgen fState r
            let newState = mState', fState'
            do
                let stateType = typedefof<Tuple<_,_>>.MakeGenericType(TypeInfo.get mState', TypeInfo.get fState')
                TypeInfo.set stateType newState
            fOut, newState
        |> Gen

    let inline ofValue x =
        Gen <| fun s r -> x,nostate

    let inline read<'r> : Gen<'r,_,'r> =
        Gen <| fun s r -> r,nostate

    let inline statesAndValue (Gen g) =
        Gen <| fun s r ->
            let v,s' = g s r
            {| inState = s; value = v; outState = s' |}, s'

    // TODO: Some of them should not be auto-opened
    let inline map proj (Gen g) = 
        Gen <| fun s r ->
            let o, (s: 's) = g s r
            do TypeInfo.set typeof<'s> s
            proj o, s

    let inline preserve (factory: unit -> 's) = 
        Gen <| fun s r ->
            let (state: 's) = s |> Option.defaultWith factory
            do TypeInfo.set typeof<'s> s
            state,state

    let inline ofMutable<'v,'r> (initialValue: 'v) =
        Gen <| fun s (r: 'r) ->
            let s : 's = s |> Option.defaultWith (fun () -> ref initialValue)
            do TypeInfo.set typeof<'s> s
            let setter = fun value -> s.contents <- value
            (s.contents, setter), s

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
