namespace Vide

// TODO: generalize this again
// Why we return option(!) of 's? -> Because of else branch / zero:
// See elseForget: The state type of the else branch has to be the same
// state type used in the if branch, but: It has no value.
// So it must be an Option<'s>.

#if USE_SINGLECASEDU_WITHOUT_INLINEIFLAMBDA

type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

[<AutoOpen>]
module VideFunction =
    let inline mkVide v = Vide v
    let inline runVide (Vide v) = v

module IILShadowing =
    type InlineIfLambdaAttribute() = inherit System.Attribute()

#else

type Vide<'v,'s,'c> = ('s option -> 'c -> 'v * 's option)

[<AutoOpen>]
module VideFunction =
    let inline mkVide<'v,'s,'c> ([<InlineIfLambda>] v: Vide<'v,'s,'c>) = v
    let inline runVide<'v,'s,'c> ([<InlineIfLambda>] v: Vide<'v,'s,'c>) = v

module IILShadowing =
    type InlineIfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute

#endif

open IILShadowing

module Vide =

    // Preserves the first value given and discards subsequent values.
    let preserveValue<'v,'c> (x: 'v) =
        mkVide <| fun s (ctx: 'c) ->
            let s = s |> Option.defaultValue x
            s, Some s
    
    let preserveWith<'v,'c> (x: unit -> 'v) =
        mkVide <| fun s (ctx: 'c) ->
            let s = s |> Option.defaultWith x
            s, Some s
    
    // TODO: Think about which function is "global" and module-bound
    let inline map ([<InlineIfLambda>] proj: 'v1 -> 'v2) (v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
        mkVide <| fun s ctx ->
            let v,s = (runVide v) s ctx
            proj v, s

    // ----- zero:
    // why are there 2 'zero' functions? -> see comment in "VideBuilder.Zero"
    // ------------------

    [<GeneralizableValue>]
    let zeroFixedState<'c> : Vide<unit,unit,'c> =
        mkVide <| fun s ctx -> (),None

    [<GeneralizableValue>]
    let zeroAdaptiveState<'s,'c> : Vide<unit,'s,'c> =
        mkVide <| fun s ctx -> (),s

    // ------------------

    [<GeneralizableValue>]
    let ctx<'c> : Vide<'c,unit,'c> =
        mkVide <| fun s ctx -> ctx,None

    let inline bind<'v1,'s1,'v2,'s2,'c>
        ([<InlineIfLambda>] f: 'v1 -> Vide<'v2,'s2,'c>)
        (m: Vide<'v1,'s1,'c>)
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        mkVide <| fun s ctx ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let mv,ms = (runVide m) ms ctx
            let f = runVide (f mv)
            let fv,fs = f fs ctx
            fv, Some (ms,fs)

    let inline ofSeqWithDefault ([<InlineIfLambda>] defaultFunc) (sequence: seq<_>) =
        mkVide <| fun s ctx ->
            let enumerator =
                let newEnum () =
                    let x = sequence.GetEnumerator()
                    if not (x.MoveNext()) then defaultFunc () else x
                match s with
                | None -> newEnum ()
                | Some (x: System.Collections.Generic.IEnumerator<_>) ->
                    if x.MoveNext() then x else newEnum ()
            enumerator.Current, Some enumerator
    
    let ofSeqWithDefaultValue defaultValue (sequence: seq<_>) =
        ofSeqWithDefault (fun () -> defaultValue) sequence
    
    let ofSeq (sequence: seq<_>) =
        ofSeqWithDefault (fun () -> failwith "Empty sequences are not supported.") sequence

    // TODO: ofList / ofArray

module BuilderBricks =
    let inline bind<'v1,'s1,'v2,'s2,'c>
        (
            m: Vide<'v1,'s1,'c>,
            [<InlineIfLambda>] f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide.bind f m

    let return'<'v,'c>
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        mkVide <| fun s ctx -> x,None

    let yield'<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        v

    // -----------------------
    // Zero:
    // -----------------------
    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // We cannot have both, which means: We cannot have "if"s without "else".
    // This is ok (and not unfortunate), because the developer has to make a
    // decision about what should happen: "elseForget" or "elsePreserve".
    // -----------------------
    
    let zeroFixedState<'c>
        ()
        : Vide<unit,unit,'c>
        = Vide.zeroFixedState<'c>

    let zeroAdaptiveState<'s,'c>
        ()
        : Vide<unit,'s,'c>
        = Vide.zeroAdaptiveState<'s,'c>

    let inline delay<'v,'s,'c>
        ([<InlineIfLambda>] f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        f ()

    // TODO
    // those 2 are important for being able to combine
    // HTML elements, returns and asyncs in arbitrary order
    //member _.Combine
    //    (
    //        a: Vide<unit,'s1,'c>,
    //        b: Vide<'v,'s2,'c>
    //    )
    //    : Vide<'v,'s1 option * 's2 option,'c>
    //    =
    //    combine a b snd
    //member _.Combine
    //    (
    //        a: Vide<'v,'s1,'c>,
    //        b: Vide<unit,'s2,'c>
    //    )
    //    : Vide<'v,'s1 option * 's2 option,'c>
    //    =
    //    combine a b fst
    let combine<'v1,'s1,'v2,'s2,'c>
        (
           a: Vide<'v1,'s1,'c>,
           b: Vide<'v2,'s2,'c>
        )
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        mkVide <| fun s ctx ->
            let sa,sb =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let va,sa = (runVide a) sa ctx
            let vb,sb = (runVide b) sb ctx
            vb, Some (sa,sb)

    // 'for' is really a NodeModel, speific thing, so I moved it there.
    // let inline for'<'a,'v,'s,'c when 'a: comparison>
    //     (
    //         inpElems: seq<'a>,
    //         [<InlineIfLambda>] body: 'a -> Vide<'v,'s,'c>
    //     ) 
    //     : Vide<'v list, ResizeArray<'s option>, 'c>
    //     =
    //     mkVide <| fun s ctx ->

// For value restriction and other resolution issues, it's better to
// move these (remaining) builder methods "as close as possible" to the builder class that's
// most specialized.
[<AbstractClass>]
type VideBaseBuilder() = class end

[<AutoOpen>]
module Keywords =
    
    [<GeneralizableValue>]
    let elsePreserve<'s,'c> : Vide<unit,'s,'c> =
        mkVide <| fun s ctx -> (),s

    [<GeneralizableValue>]
    let elseForget<'s,'c> : Vide<unit,'s,'c> = 
        mkVide <| fun s ctx -> (),None
