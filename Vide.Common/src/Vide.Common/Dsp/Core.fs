module Vide.DSP

open Vide

type DspBuilder() =
    inherit VideBaseBuilder()
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zero()
    member _.Return(x) = BuilderBricks.return' x
    member _.ReturnFrom(v) = v

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

    let inline toEvaluable
        (getCtx: int -> 'c)
        (generator: Vide<'v,'s,'c>) 
        =
        Fx.toEvaluable getCtx (fun () -> generator)

    let inline toSeq
        (getCtx: int -> 'c)
        (generator: Vide<'v,'s,'c>) 
        =
        let evaluable = toEvaluable getCtx generator
        seq { while true do evaluable () }

    let eval n (v: Vide<_,_,_>) =
        toSeq ignore v |> Seq.take n |> Seq.toList

    let inline counter inclusiveFrom incrementBy = 
        mkVide <| fun s ctx ->
            let curr = s |> Option.defaultValue inclusiveFrom
            curr, Some (curr + incrementBy)

    // TODO: Reset and other control functions (see LocSta)

module Operators =
    let ( >=> ) = Fx.kleisli
    let ( |=> ) = Gen.kleisli
    let ( +-> ) initial f = initial |> feedback f
