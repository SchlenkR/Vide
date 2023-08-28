module Vide.DSP

open System
open Vide

type DspBuilder() =
    inherit VideBaseBuilder()
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zero()
    member _.Return(x) = BuilderBricks.return' x
    member _.ReturnFrom(v) = v

let dsp = DspBuilder()

let inline kleisli 
    ([<InlineIfLambda>] f: 'a -> Vide<'b,_,'ctx>) 
    ([<InlineIfLambda>] g: 'b -> Vide<'c,_,'ctx>) 
    : 'a -> Vide<'c,_,'ctx> 
    =
    fun (x: 'a) ->
        dsp {
            let f = f x
            let! fv = f
            return! g fv
        }

let inline kleisliPipe
    ([<InlineIfLambda>] f: Vide<'a,_,'ctx>)
    ([<InlineIfLambda>] g: 'a -> Vide<'b,_,'ctx>)
    : Vide<'b,_,'ctx>
    =
    dsp {
        let! fv = f
        return! g fv
    }

let inline private binOpBoth
    (left: Vide<'v1,'s1,'c>)
    (right: Vide<'v2,'s2,'c>)
    ([<InlineIfLambda>] f)
    =
    dsp {
        let! l = left
        let! r = right
        return f l r 
    }

// TODO
// type Vide<'v,'s,'c> with
//     static member inline (+) (left, right) = binOpBoth left right (+)
//     static member inline (-) (left, right) = binOpBoth left right (-)
//     static member inline (*) (left, right) = binOpBoth left right (*)
//     static member inline (/) (left, right) = binOpBoth left right (/)
//     static member inline (%) (left, right) = binOpBoth left right (%)

let inline private binOpLeft left right f =
    dsp {
        let l = left
        let! r = right
        return f l r
    }

// TODO
// type Vide<'v,'s,'c> with
//     static member inline (+) (left, right) = binOpLeft left right (+)
//     static member inline (-) (left, right) = binOpLeft left right (-)
//     static member inline (*) (left, right) = binOpLeft left right (*)
//     static member inline (/) (left, right) = binOpLeft left right (/)
//     static member inline (%) (left, right) = binOpLeft left right (%)

let inline private binOpRight
    (left: Vide<'v1,'s1,'c>)
    (right: Vide<'v2,'s2,'c>)
    ([<InlineIfLambda>] f)
    =
    dsp {
        let! l = left
        let r = right
        return f l r
    }

// TODO
// type Vide<'v,'s,'c> with
//     static member inline (+) (left, right) = binOpRight left right (+)
//     static member inline (-) (left, right) = binOpRight left right (-)
//     static member inline (*) (left, right) = binOpRight left right (*)
//     static member inline (/) (left, right) = binOpRight left right (/)

[<GeneralizableValue>]
let ctx<'s,'c> : Vide<'c,'s,'c> =
    mkVide <| fun _ ctx -> ctx,None

let feedback (f: 'fdb -> 'c -> Vide<'v * 'fdb,'s,'c>) (seed: 'fdb) =
    mkVide <| fun s ctx ->
        let fdb,s =
            match s with
            | None -> seed,None
            | Some (lastFdb,s) -> lastFdb,s
        let (newv,newf),news = runVide (f fdb ctx) s ctx
        newv, Some (newf, news)
let ( ++> ) seed f = feedback f seed

// TODO: CE support for feedback

let inline toEvaluableEffect 
    (getCtx: int -> 'c)
    ([<InlineIfLambda>] effect: 'v1 -> Vide<'v2,'s,'c>)
    =
    let mutable i = 0
    let mutable lastState = None
    fun inputValue ->
        let vide = runVide (effect inputValue)
        let xv,xs = vide lastState (getCtx i)
        do i <- i + 1
        do lastState <- xs
        xv

let inline toSeqEffect 
    (getCtx: int -> 'c)
    ([<InlineIfLambda>] effect: 'v1 -> Vide<'v2,'s,'c>)
    =
    let evaluable = toEvaluableEffect getCtx effect
    fun inputValues -> seq { for x in inputValues do evaluable x }

let inline toEvaluableGenerator
    (getCtx: int -> 'c)
    (generator: Vide<'v,'s,'c>) 
    =
    let effect = fun () -> generator
    toEvaluableEffect getCtx effect

let inline toSeqGenerator
    (getCtx: int -> 'c)
    (generator: Vide<'v,'s,'c>) 
    =
    let evaluable = toEvaluableGenerator getCtx generator
    seq { while true do evaluable () }

let evalEffect inputValues v =
    toSeqEffect ignore v inputValues |> Seq.toList

let evalGenerator n (v: Vide<_,_,_>) =
    toSeqGenerator ignore v |> Seq.take n |> Seq.toList

module Operators =
    let ( >=> ) = kleisli
    let ( |=> ) = kleisliPipe
