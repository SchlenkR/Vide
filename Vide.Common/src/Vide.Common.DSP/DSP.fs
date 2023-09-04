namespace Vide.DSP

open Vide

type DspBuilder() =
    inherit VideBaseBuilder()
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zeroFixedState()
    member _.Return(x) = BuilderBricks.return' x
    member _.ReturnFrom(v) = v

[<AutoOpen>]
module TopLevels =
    let dsp = DspBuilder()

    let feedback (initial: 'fdb) (f: 'fdb -> 'c -> Vide<'v * 'fdb,'s,'c>) =
        mkVide <| fun s ctx ->
            let fdb,s =
                match s with
                | None -> initial,None
                | Some (lastFdb,s) -> lastFdb,s
            let (newv,newf),news = runVide (f fdb ctx) s ctx
            newv, Some (newf, news)

    // TODO: CE support for feedback

module Fx =
    let inline kleisli
        ([<InlineIfLambda>] f: _ -> Vide<_,_,'c>) 
        ([<InlineIfLambda>] g: _ -> Vide<_,_,'c>) 
        : _ -> Vide<_,_,'c> 
        =
        fun (x: 'v1) ->
            dsp {
                let f = f x
                let! fv = f
                return! g fv
            }

    module Eval =
        let inline toEvaluable 
            (getCtx: int -> 'c)
            ([<InlineIfLambda>] fx: 'v1 -> Vide<'v2,'s,'c>)
            =
            let mutable currCycle = 0
            let mutable lastState = None
            fun inputValue ->
                let vide = runVide (fx inputValue)
                let xv,xs = vide lastState (getCtx currCycle)
                do currCycle <- currCycle + 1
                do lastState <- xs
                xv

        let inline toSeq 
            (getCtx: int -> 'c)
            ([<InlineIfLambda>] fx: 'v1 -> Vide<'v2,'s,'c>)
            =
            let evaluable = toEvaluable getCtx fx
            fun inputValues -> seq { for x in inputValues do evaluable x }

        let eval inputValues v =
            toSeq ignore v inputValues |> Seq.toList

    /// Delays a given value by 1 cycle.
    let inline delay defaultValue input =
        mkVide <| fun s ctx ->
            let v =
                match s with
                | None -> defaultValue
                | Some s -> s
            v, Some input

    // TODO:
    // let delayBy seed samples =
    //     fun p _ =

    let inline slope fromValue toValue input =
        mkVide <| fun s ctx ->
            let res =
                match s with
                | None -> false
                | Some lastValue -> lastValue = fromValue && input = toValue
            res, Some input

module Gen =
    let inline kleisli
        ([<InlineIfLambda>] vide: Vide<_,_,'c>)
        ([<InlineIfLambda>] g: _ -> Vide<'v2,_,'c>)
        : Vide<_,_,'c>
        =
        dsp {
            let! v = vide
            return! g v
        }

    module Eval =
        let inline toEvaluable
            (getCtx: int -> 'c)
            (gen: Vide<'v,'s,'c>) 
            =
            Fx.Eval.toEvaluable getCtx (fun () -> gen)

        let inline toSeq
            (getCtx: int -> 'c)
            (gen: Vide<'v,'s,'c>) 
            =
            let evaluable = toEvaluable getCtx gen
            seq { while true do evaluable () }

        let eval n (v: Vide<_,_,_>) =
            toSeq ignore v |> Seq.take n |> Seq.toList

    let inline counter inclusiveFrom incrementBy = 
        mkVide <| fun s ctx ->
            let curr = s |> Option.defaultValue inclusiveFrom
            curr, Some (curr + incrementBy)
    
    let inline repeat<'v,'c> (values: 'v list) =
        mkVide <| fun s (ctx: 'c) ->
            let idx = s |> Option.defaultValue 0
            let v = values.[idx % values.Length]
            v, Some (idx + 1)
    
    let [<GeneralizableValue>] toggle01f<'c> = repeat<_,'c> [ 0.0; 1.0 ]
    let [<GeneralizableValue>] toggle01i<'c> = repeat<_,'c> [ 0; 1 ]
    let [<GeneralizableValue>] toggleTrueFalse<'c> = repeat<_,'c> [ true; false ]
    
    let [<GeneralizableValue>] toggle10f<'c> = repeat<_,'c> [ 1.0; 0.0 ]
    let [<GeneralizableValue>] toggle10i<'c> = repeat<_,'c> [ 1; 0 ]
    let [<GeneralizableValue>] toggleFalseTrue<'c> = repeat<_,'c> [ false; true]

    // TODO: Reset and other control functions (see LocSta)

module Operators =
    let ( >=> ) = Fx.kleisli
    let ( |=> ) = Gen.kleisli
    let ( +-> ) initial f = initial |> feedback f
