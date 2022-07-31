
(*
A hint for the type argument names:
    * 'o: output of a Gen.
    * 'v: value; the actual value emitted by a Gen - usually corresponds to the output of a Gen.
    * 'f: feedback
    * 's: state
For the API surface, names like 'value or 'state are used instead of chars.
*)

// TODO: InlineIfLambda, ValueOptions, Structs
namespace LocSta

// TODO: Maybe spare of arithmetics and use func only + [<InlineIfLambda>] ?
type Gen<'o,'s> = Gen of ('s option -> 'o)

// TODO: Is it really a good idea generalizing Res instead of using disjoint results for Feed and Loop?
[<RequireQualifiedAccess>]
type Res<'v,'s> =
    | Continue of 'v list * 's
    | Stop of 'v list

// TODO: Make: FeedType/FeedState and also LoopType/LoopState
[<RequireQualifiedAccess>]
type LoopState<'s> =
    | Update of 's
    | KeepLast
    | Reset
type LoopRes<'o,'s> = Res<'o, LoopState<'s>>
type LoopGen<'o,'s> = Gen<LoopRes<'o,'s>, 's>

[<RequireQualifiedAccess>]
type FeedType<'f> =
    | Update of 'f
    | KeepLast
    | Reset
    | ResetFeedback
    | ResetDescendants of 'f

type FeedState<'s,'f> = FeedState of 's option * FeedType<'f>
type FeedRes<'o,'s,'f> = Res<'o, FeedState<'s,'f>>
type FeedGen<'o,'s,'f> = Gen<FeedRes<'o,'s,'f>, 's> 

// TODO: document both cases, BUT: Init is lazy anyway
type Init<'f> =
    | Init of 'f
    | InitWith of (unit -> 'f)

type Fx<'i,'o,'s> = 'i -> Gen<'o,'s>

[<Struct>]
type BindState<'sm, 'sk, 'm> =
    { mstate: 'sm option       // this is optional because m can have no state by just yielding Stop
      kstate: 'sk option       // means: k evaluates to a constant; "return" uses this.
      mleftovers: 'm list
      isStopped: bool }


// TODO: Should this be private?
/// Convenience for working directly with Gen funcs.
module Res =
    module Loop =
        let emitMany values state = Res.Continue (values, LoopState.Update state)
        let emitManyAndKeepLast values = Res.Continue (values, LoopState.KeepLast)
        let emitManyAndReset values = Res.Continue (values, LoopState.Reset)
        let emitManyAndStop values = Res.Stop values

        let emit value state = emitMany [value] state
        let emitAndKeepLast value = emitManyAndKeepLast [value]
        let emitAndReset value = emitManyAndReset [value]
        let emitAndStop value = emitManyAndStop [value]

        let skip state = emitMany [] state
        let skipAndKeepLast<'v,'s> = emitManyAndKeepLast [] : Res<'v, LoopState<'s>>
        let skipAndReset<'v,'s> = emitManyAndReset [] : Res<'v, LoopState<'s>>
        let stop<'v,'s> = emitManyAndStop [] : Res<'v, LoopState<'s>>

        let zero<'v,'s> = skipAndKeepLast<'v,'s>

    module Feed =
        let inline private cont values feedType = Res.Continue (values, FeedState (None, feedType))
        
        let emitMany values feedback = cont values (FeedType.Update feedback)
        let emitManyAndKeepLast values = cont values (FeedType.KeepLast)
        let emitManyAndReset values = cont values FeedType.Reset
        let emitManyAndResetFeedback values = cont values FeedType.ResetFeedback
        let emitManyAndResetDescendants values feedback = cont values (FeedType.ResetDescendants feedback)
        let emitManyAndStop values = Res.Stop values

        let emit value feedback = emitMany [value] feedback
        let emitAndKeepLast value = emitManyAndKeepLast [value]
        let emitAndReset value = emitManyAndReset [value]
        let emitAndResetFeedback value = emitManyAndResetFeedback [value]
        let emitAndResetDescendants value feedback = emitManyAndResetDescendants [value] feedback
        let emitAndStop value = emitManyAndStop [value]

        let skip feedback = emitMany [] feedback
        let skipAndKeepLast<'v,'s,'f> = emitManyAndKeepLast [] : Res<'v, FeedState<'s,'f>>
        let skipAndReset<'v,'s,'f> = emitManyAndReset [] : Res<'v, FeedState<'s,'f>>
        let skipAndResetFeedback<'v,'s,'f> = emitManyAndResetFeedback [] : Res<'v, FeedState<'s,'f>>
        let skipAndResetDescendants feedback = emitManyAndResetDescendants [] feedback
        let stop<'v,'s,'f> = emitManyAndStop [] : Res<'v, FeedState<'s,'f>>

        let zero<'v,'s,'f> = skipAndKeepLast<'v,'s,'f>


/// Vocabulary for Return of loop CE.
module Loop =
    type [<Struct>] Emit<'value> = Emit of 'value
    type [<Struct>] EmitAndReset<'value> = EmitAndReset of 'value
    type [<Struct>] EmitAndStop<'value> = EmitAndStop of 'value

    type [<Struct>] EmitMany<'value> = EmitMany of 'value list
    type [<Struct>] EmitManyAndReset<'value> = EmitManyAndReset of 'value list
    type [<Struct>] EmitManyAndStop<'value> = EmitManyAndStop of 'value list

    type [<Struct>] Skip = Skip
    type [<Struct>] SkipAndReset = SkipAndReset
    type [<Struct>] Stop = Stop


/// Vocabulary for Return of feed CE.
module Feed =
    type [<Struct>] Emit<'value, 'feedback> = Emit of 'value * 'feedback
    type [<Struct>] EmitAndKeepLast<'value> = EmitAndKeepLast of 'value
    type [<Struct>] EmitAndReset<'value> = EmitAndReset of 'value
    type [<Struct>] EmitAndResetFeedback<'value> = EmitAndResetFeedback of 'value
    type [<Struct>] EmitAndResetDescendants<'value, 'feedback> = EmitAndResetDescendants of 'value * 'feedback
    type [<Struct>] EmitAndStop<'value> = EmitAndStop of 'value

    type [<Struct>] EmitMany<'value, 'feedback> = EmitMany of 'value list * 'feedback
    type [<Struct>] EmitManyAndKeepLast<'value> = EmitManyAndKeepLast of 'value list
    type [<Struct>] EmitManyAndReset<'value> = EmitManyAndReset of 'value list
    type [<Struct>] EmitManyAndResetFeedback<'value> = EmitManyAndResetFeedback of 'value list
    type [<Struct>] EmitManyAndResetDescendants<'value, 'feedback> = EmitManyAndResetDescendants of 'value list * 'feedback
    type [<Struct>] EmitManyAndStop<'value> = EmitManyAndStop of 'value list

    type [<Struct>] Skip<'feedback> = Skip of 'feedback
    type [<Struct>] SkipAndKeepLast = SkipAndKeepLast
    type [<Struct>] SkipAndReset = SkipAndReset
    type [<Struct>] SkipAndResetFeedback = SkipAndResetFeedback
    type [<Struct>] SkipAndResetDescendants<'feedback> = SkipAndResetDescendants of 'feedback
    type [<Struct>] Stop = Stop


// TODO: make some things internal and expose them explicitly via Gen type
module Gen =

    let run (gen: Gen<_,_>) = let (Gen b) = gen in b


    // --------
    // Gen creation
    // --------

    let inline createGen ([<InlineIfLambda>] f) = Gen f
    let inline createLoop ([<InlineIfLambda>] f) : LoopGen<_,_> = Gen f
    let inline createFeed ([<InlineIfLambda>] f) : FeedGen<_,_,_> = Gen f

    
    // --------
    // Active Recognizers
    // --------

    open System.ComponentModel

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let (|LoopStateToOption|) defaultState currState =
        match currState with
        | LoopState.Update s -> Some s
        | LoopState.KeepLast -> defaultState
        | LoopState.Reset -> None


    // --------
    // bind
    // --------

    // TODO: Bräuchte buildState nicht auch den lastState? Und as TODO von unten - welche Rolle spielt das?
    let inline internal bindLoopWhateverGen 
        ([<InlineIfLambda>] evalk)
        ([<InlineIfLambda>] buildSkip)
        ([<InlineIfLambda>] createWhatever)
        m
        =
        fun state ->
            let evalmres mres lastMState lastKState isStopped =
                match mres with
                | Res.Continue (mval :: mleftovers, LoopStateToOption lastMState mstate) ->
                    evalk mval mstate mleftovers lastKState isStopped
                | Res.Continue ([], LoopStateToOption lastMState mstate) ->
                    let state = { mstate = mstate; kstate = lastKState; mleftovers = []; isStopped = isStopped }
                    // TODO: why "None" in case of Res.Continue?
                    Res.Continue ([], buildSkip state)
                | Res.Stop (mval :: mleftovers) ->
                    evalk mval lastMState mleftovers lastKState isStopped
                | Res.Stop [] ->
                    Res.Stop []
            match state with
            | Some { mstate = lastMState; mleftovers = x :: xs; kstate = lastKState; isStopped = isStopped } ->
                evalk x lastMState xs lastKState isStopped
            | Some { mleftovers = []; isStopped = true } ->
                Res.Stop []
            | Some { mstate = lastMState; mleftovers = []; kstate = kstate } ->
                evalmres (run m lastMState) lastMState kstate false
            | None ->
                evalmres (run m None) None None false
        |> createWhatever

    let inline bind
        ([<InlineIfLambda>] k: 'o1 -> LoopGen<'o2,'s2>)
        (m: LoopGen<'o1,'s1>)
        : LoopGen<'o2, BindState<'s1,'s2,'o1>>
        =
        let evalk mval mstate mleftovers lastKState isStopped =
            match run (k mval) lastKState with
            | Res.Continue (kvalues, kstate) ->
                let newState kstate = { mstate = mstate; kstate = kstate; mleftovers = mleftovers; isStopped = isStopped }
                match kstate with
                | LoopState.Update kstate ->
                    Res.Continue (kvalues, LoopState.Update (newState (Some kstate)))
                | LoopState.KeepLast ->
                    Res.Continue (kvalues, LoopState.Update (newState lastKState))
                | LoopState.Reset ->
                    Res.Continue (kvalues, LoopState.Reset)
            | Res.Stop kvalues ->
                Res.Stop kvalues
        let buildSkip state = LoopState.Update state
        bindLoopWhateverGen evalk buildSkip createLoop m

    let inline internal bindLoopFeedFeed
        ([<InlineIfLambda>] k: 'o1 -> FeedGen<'o2,'s2,'f>)
        (m: LoopGen<'o1,'s1>)
        : FeedGen<'o2,_,'f> // TODO: _
        =
        let evalk mval mstate mleftovers lastKState isStopped =
            match run (k mval) lastKState with
            | Res.Continue (kvalues, FeedState (kstate, feedState)) ->
                let state = { mstate = mstate; kstate = kstate; mleftovers = mleftovers; isStopped = isStopped }
                Res.Continue (kvalues, FeedState (Some state, feedState))
            | Res.Stop kvalues ->
                Res.Stop kvalues
        let buildSkip state = FeedState (Some state, FeedType.KeepLast)
        bindLoopWhateverGen evalk buildSkip createFeed m

    let inline internal bindInitFeedLoop
        ([<InlineIfLambda>] k: 'f -> FeedGen<'o,'s,'f>)
        (m: Init<'f>)
        : LoopGen<_,_>
        =
        fun state ->
            let getInitial () =
                match m with
                | Init m -> m
                | InitWith f -> f()
            let evalk lastFeed lastKState =
                match run (k lastFeed) lastKState with
                | Res.Continue (kvalues, FeedState (kstate, feedback)) ->
                    let feedback,kstate =
                        match feedback with                                                                                                                                                                                                                                             
                        | FeedType.Update feedback -> Some feedback, kstate
                        | FeedType.KeepLast -> Some lastFeed, kstate
                        | FeedType.Reset -> None, None
                        | FeedType.ResetFeedback -> None, kstate
                        | FeedType.ResetDescendants feedback -> Some feedback, None
                    let state = { mstate = feedback; kstate = kstate; mleftovers = []; isStopped = false }
                    Res.Continue (kvalues, LoopState.Update state)
                | Res.Stop kvalues ->
                    Res.Stop kvalues
            match state with
            | Some { isStopped = true } ->
                Res.Stop []
            | None ->
                evalk (getInitial()) None
            | Some { mstate = None; kstate = kstate } ->
                evalk (getInitial()) kstate
            | Some { mstate = Some feedback; kstate = kstate } ->
                evalk feedback kstate
        |> createLoop


    // --------
    // create of values
    // --------

    let inline internal returnLoopRes res = createLoop (fun _ -> res)
    let inline internal returnFeedRes res = createFeed (fun _ -> res)

    let ofRepeatingValues<'v, 's> values : LoopGen<'v,'s> = returnLoopRes (Res.Continue (values, LoopState.KeepLast))
    let ofRepeatingValue<'v, 's> value : LoopGen<'v,'s> = ofRepeatingValues [value]
    let ofOneTimeValues<'v, 's> values : LoopGen<'v,'s> = returnLoopRes (Res.Stop values)
    let ofOneTimeValue<'v, 's> value : LoopGen<'v,'s> = ofOneTimeValues [value]


    // --------
    // create of seq / list
    // --------

    // TODO: think about dropping ofSeq support completely
    let ofSeqOneByOne (s: seq<_>) =
        fun enumerator ->
            let enumerator = enumerator |> Option.defaultWith (fun () -> s.GetEnumerator())
            match enumerator.MoveNext() with
            | true -> Res.Loop.emit enumerator.Current enumerator
            | false -> Res.Loop.stop
        |> createLoop

    // TODO: Improve naming

    /// Emits the head of the list and retains the excess or stops on an empty list.
    let ofListOneByOne (list: list<_>) =
        fun l ->
            let l = l |> Option.defaultValue list
            match l with
            | x::xs -> Res.Loop.emit x xs
            | [] -> Res.Loop.stop
        |> createLoop

    /// Emits the complete list or stopps on an empty list.
    let ofListAllAtOnce (list: list<_>) =
        fun _ ->
            match list with
            | [] -> Res.Loop.stop
            | l -> Res.Loop.emitManyAndKeepLast l
        |> createLoop


    // --------
    // combine
    // --------

    type CombineInfo<'sa, 'sb> =
        { astate: 'sa option
          bstate: 'sb option }

    let inline internal combineLoop
        (a: LoopGen<'o, 'sa>)
        ([<InlineIfLambda>] b: unit -> LoopGen<'o, 'sb>)
        : LoopGen<'o, CombineInfo<'sa,'sb>>
        =
        fun state ->
            let state =  state |> Option.defaultValue { astate = None; bstate = None }
            let ares = run a state.astate
            match ares with
            | Res.Continue (avalues, LoopStateToOption state.astate astate) ->
                let bres = run (b()) state.bstate
                match bres with
                | Res.Continue (bvalues, LoopStateToOption state.bstate bstate) ->
                    Res.Loop.emitMany (avalues @ bvalues) { astate = astate; bstate = bstate }
                | Res.Stop bvalues ->
                    Res.Loop.emitManyAndStop (avalues @ bvalues)
            | Res.Stop avalues ->
                Res.Stop avalues
        |> createLoop

    // Wie genau verhält es sich, wenn ich 2 feeds combine (und 'fa, 'fb, 'fc)?
    let inline internal combineFeed
        (a: FeedGen<'o, 'sa, 'f>)
        ([<InlineIfLambda>] b: unit -> FeedGen<'o, 'sb, 'f>)
        : FeedGen<'o, CombineInfo<'sa,'sb>, 'f>
        =
        fun state ->
            let state =  state |> Option.defaultValue { astate = None; bstate = None }
            match run a state.astate with
            // TODO: document this: 'afeedback' is unused, which means: the last emitted feedback is used when combining
            | Res.Continue (avalues, FeedState (astate, afeedback))->
                match run (b()) state.bstate with
                | Res.Continue (bvalues, FeedState (bstate, bfeedback)) as b ->
                    let finalFeedback =
                        // why? we can have multiple 'if-no-else(zero)'s combined together
                        if b = Res.Feed.zero then afeedback else bfeedback
                    let state = { astate = astate; bstate = bstate }
                    Res.Continue (avalues @ bvalues, FeedState (Some state, finalFeedback))
                | Res.Stop bvalues ->
                    Res.Stop (avalues @ bvalues)
            | Res.Stop avalues ->
                Res.Stop avalues
        |> createFeed


    // -------
    // evaluation / transform to other domains
    // -------
    
    // TODO: same pattern (resumeOrStart, etc.) as in Gen also for Fx

    type Evaluable<'a>([<InlineIfLambda>] f: unit -> 'a) =
        member _.Evaluate() = f()

    let toEvaluable (g: LoopGen<_,'s>) =
        let f = run g
        let mutable state = None
        let mutable resume = true
        let mutable remainingValues = []
        let rec getNext() =
            match remainingValues, resume with
            | x :: xs, _ ->
                remainingValues <- xs
                Some x
            | [], true ->
                match f state with
                | Res.Continue (values, LoopStateToOption state fstate) ->
                    state <- fstate
                    remainingValues <- values
                    getNext()
                | Res.Stop values ->
                    resume <- false
                    remainingValues <- values
                    getNext()
            | _ -> None
        Evaluable(getNext)

    // TODO: use toEvaluable
    let toSeq (g: LoopGen<_,'s>) : seq<_> =
        let f = run g
        let mutable state = None
        let mutable resume = true
        seq {
            while resume do
                match f state with
                | Res.Continue (values, LoopStateToOption state fstate) ->
                    state <- fstate
                    yield! values
                | Res.Stop values ->
                    resume <- false
                    yield! values
        }

    // TODO: quite redundant with toSeq, but wrapping it's 'g' seems inefficient
    // TODO: use toEvaluable
    let inline toSeqFx
        ([<InlineIfLambda>] fx: 'i -> LoopGen<'o,'s>)
        : seq<'i> -> seq<'o>
        =
        let mutable state = None
        let mutable resume = true
        fun inputValues ->
            let enumerator = inputValues.GetEnumerator()
            seq {
                while resume && enumerator.MoveNext() do
                    match run (fx enumerator.Current) state with
                    | Res.Continue (values, LoopStateToOption state fstate) ->
                        state <- fstate
                        yield! values
                    | Res.Stop values ->
                        resume <- false
                        yield! values
            }
    
    let toList gen =
        toSeq gen |> Seq.toList

    let toListn numOfElements gen =
        toSeq gen |> Seq.truncate numOfElements |> Seq.toList

    let inline toListFx ([<InlineIfLambda>] fx) input =
        input |> toSeqFx fx |> Seq.toList

    let toFx (gen: Gen<'s, 'o>) : Fx<unit, 's, 'o> =
        fun () -> gen


    // --------
    // Builder
    // --------

    type BaseBuilder() =
        member _.ReturnFrom(x) = x
        member _.YieldFrom(x) = ofListAllAtOnce x
        member _.Delay(delayed) = delayed
        member inline _.Run([<InlineIfLambda>] delayed) = delayed ()
        member _.For(list: list<_>, body) = list |> toListFx body |> ofListAllAtOnce
        // TODO: member _.For(sequence: seq<'a>, body) = ofSeq sequence |> onStopThenSkip |> bind body

    type LoopBuilder() =
        inherit BaseBuilder()

        member inline _.Bind(m, [<InlineIfLambda>] f) = bind f m
        member _.Combine(x, delayed) = combineLoop x delayed
        
        member _.Zero() = returnLoopRes Res.Loop.zero

        member _.Yield(value) : LoopGen<_,_> = returnLoopRes (Res.Loop.emitAndKeepLast value)

        // TODO: Die müssen alle in coreLoopTests abgetestet sein
        member _.Return(Loop.Emit value) = returnLoopRes (Res.Loop.emitAndKeepLast value)
        member _.Return(Loop.EmitAndReset value) = returnLoopRes (Res.Loop.emitAndReset value)
        member _.Return(Loop.EmitAndStop value) = returnLoopRes (Res.Loop.emitAndStop value)

        member _.Return(Loop.EmitMany values) = returnLoopRes (Res.Loop.emitManyAndKeepLast values)
        member _.Return(Loop.EmitManyAndReset values) = returnLoopRes (Res.Loop.emitManyAndReset values)
        member _.Return(Loop.EmitManyAndStop values) = returnLoopRes (Res.Loop.emitManyAndStop values)

        member _.Return(Loop.Skip) = returnLoopRes Res.Loop.skipAndKeepLast
        member _.Return(Loop.SkipAndReset) = returnLoopRes Res.Loop.skipAndReset
        member _.Return(Loop.Stop) = returnLoopRes Res.Loop.stop
        
    type FeedBuilder() =
        inherit BaseBuilder()
               
        member inline _.Bind(m, [<InlineIfLambda>] f) = bindInitFeedLoop f m
        member inline _.Bind(m, [<InlineIfLambda>] f) = bind f m
        member inline _.Bind(m, [<InlineIfLambda>] f) = bindLoopFeedFeed f m

        member inline _.Combine(x, [<InlineIfLambda>] delayed) = combineLoop x delayed
        member inline _.Combine(x, [<InlineIfLambda>] delayed) = combineFeed x delayed
        
        // TODO: Die müssen alle in coreLoopTests abgetestet sein

        member _.Zero() = returnFeedRes Res.Feed.zero

        member _.Yield(value, feedback) = returnFeedRes (Res.Feed.emit value feedback)

        member _.Return(Feed.Emit (value, feedback)) = returnFeedRes (Res.Feed.emit value feedback)
        member _.Return(Feed.EmitAndKeepLast value) = returnFeedRes (Res.Feed.emitAndKeepLast value)
        member _.Return(Feed.EmitAndReset value) = returnFeedRes (Res.Feed.emitAndReset value)
        member _.Return(Feed.EmitAndResetFeedback value) = returnFeedRes (Res.Feed.emitAndResetFeedback value)
        member _.Return(Feed.EmitAndResetDescendants (value, feedback)) = returnFeedRes (Res.Feed.emitAndResetDescendants value feedback)
        member _.Return(Feed.EmitAndStop value) = returnFeedRes (Res.Feed.emitAndStop value)

        member _.Return(Feed.EmitMany (values, feedback)) = returnFeedRes (Res.Feed.emitMany values feedback)
        member _.Return(Feed.EmitManyAndKeepLast values) = returnFeedRes (Res.Feed.emitManyAndKeepLast values)
        member _.Return(Feed.EmitManyAndReset values) = returnFeedRes (Res.Feed.emitManyAndReset values)
        member _.Return(Feed.EmitManyAndResetFeedback values) = returnFeedRes (Res.Feed.emitManyAndResetFeedback values)
        member _.Return(Feed.EmitManyAndResetDescendants (values, feedback)) = returnFeedRes (Res.Feed.emitManyAndResetDescendants values feedback)
        member _.Return(Feed.EmitManyAndStop values) = returnFeedRes (Res.Feed.emitManyAndStop values)

        member _.Return(Feed.Skip feedback) = returnFeedRes (Res.Feed.skip feedback)
        member _.Return(Feed.SkipAndKeepLast) = returnFeedRes Res.Feed.skipAndKeepLast
        member _.Return(Feed.SkipAndReset) = returnFeedRes Res.Feed.skipAndReset
        member _.Return(Feed.SkipAndResetFeedback) = returnFeedRes Res.Feed.skipAndResetFeedback
        member _.Return(Feed.SkipAndResetDescendants feedback) = returnFeedRes (Res.Feed.skipAndResetDescendants feedback)
        member _.Return(Feed.Stop) = returnFeedRes Res.Feed.stop
    
    let loop = LoopBuilder()
    let feed = FeedBuilder()


    // -------
    // Kleisli composition
    // -------

    let inline pipe ([<InlineIfLambda>] g: Fx<_,_,_>) (f: Gen<_,_>) : Gen<_,_> =
        loop {
            let! f' = f
            return! g f' 
        }

    let inline pipeFx ([<InlineIfLambda>] g: Fx<_,_,_>) (f: Fx<_,_,_>): Fx<_,_,_> =
        fun x -> loop {
            let! f' = f x
            return! g f' 
        }

    
    // --------
    // map / apply / transformation
    // --------

    let inline mapValueAndState 
        ([<InlineIfLambda>] proj: 'v -> LoopState<'s> option -> 'o) 
        (inputGen: LoopGen<_,_>) 
        : LoopGen<_,_> 
        =
        fun state ->
            let mapValues values state = [ for v in values do proj v state ]
            match run inputGen state with
            | Res.Continue (values, state) ->
                Res.Continue (mapValues values (Some state), state)
            | Res.Stop values ->
                Res.Loop.emitManyAndStop (mapValues values None)
        |> createLoop

    let inline map 
        ([<InlineIfLambda>] proj) 
        (inputGen: LoopGen<_,_>) 
        =
        mapValueAndState (fun v _ -> proj v) inputGen

    let apply xGen fGen =
        loop {
            let! l' = xGen
            let! f' = fGen
            let result = f' l'
            yield result
        }
    
    // TODO: Test / Docu
    let withState (inputGen: LoopGen<_,_>) =
        mapValueAndState (fun v s -> v,s) inputGen


    // --------
    // onStop trigger
    // --------

    type OnStopThenState<'s> =
        | RunInput of 's option
        | UseDefault

    let inline onStopThenValues defaultValues (inputGen: LoopGen<_,_>) : LoopGen<_,_> =
        fun state ->
            let state = state |> Option.defaultValue (RunInput None)
            match state with
            | UseDefault ->
                Res.Continue (defaultValues, LoopState.Update UseDefault)
            | RunInput state ->
                let continueWith values state = Res.Loop.emitMany values (RunInput state)
                match run inputGen state with
                | Res.Continue (values, LoopStateToOption None state) ->
                    continueWith values state
                | Res.Stop values ->
                    Res.Loop.emitMany values UseDefault
        |> createLoop
        
    let inline onStopThenDefault defaultValue (inputGen: LoopGen<_,_>) : LoopGen<_,_> =
        onStopThenValues [defaultValue] inputGen

    let inline onStopThenSkip (inputGen: LoopGen<_,_>) : LoopGen<_,_> =
        onStopThenValues [] inputGen

    
    // -------
    // count
    // -------

    let inline count inclusiveStart increment =
        feed {
            let! curr = Init inclusiveStart
            yield curr, curr + increment
        }

    let inline countToAndDo inclusiveStart increment inclusiveEnd action =
        loop {
            let! c = count inclusiveStart increment
            match c <= inclusiveEnd with
            | true -> yield c
            | false -> return! createLoop (fun _ -> action)
        }

    let inline countTo inclusiveStart increment inclusiveEnd =
        countToAndDo inclusiveStart increment inclusiveEnd Res.Loop.stop

    let inline countToAndRepeat inclusiveStart increment inclusiveEnd =
        countToAndDo inclusiveStart increment inclusiveEnd Res.Loop.skipAndReset


    // ----------
    // control: reset / stop
    // ----------

    /// Evluates the input gen and passes it's output to the predicate function:
    /// When that returns true, the input gen is evaluated once again with an empty state.
    /// It returns the value and a bool indicating if a reset did happen.
    let inline doWhen ([<InlineIfLambda>] pred: _ -> bool) action (inputGen: LoopGen<_,_>) =
        loop {
            let! value,state = withState inputGen
            if pred value then
                do action value state
            yield value
        }

    /// Evluates the input gen and passes it's output to the predicate function:
    /// When that returns true, the input gen is evaluated once again with an empty state.
    let inline resetWhen ([<InlineIfLambda>] pred: _ -> bool) (inputGen: LoopGen<_,_>) =
        loop {
            let! value = inputGen
            if pred value
                then return Loop.EmitAndReset value
                else return Loop.Emit value
        }
    
    let inline stopWhen ([<InlineIfLambda>] pred: _ -> bool) (inputGen: LoopGen<_,_>) =
        loop {
            let! value = inputGen
            if pred value
                then return Loop.Stop
                else return Loop.Emit value
        }

    // TODO: doOnStop?

    let resetWhenStop (inputGen: LoopGen<_,_>) =
        fun state ->
            match run inputGen state with
            | Res.Stop values -> Res.Loop.emitManyAndReset values
            | x -> x
        |> createLoop


    // ----------
    // other seq-like functions
    // ----------

    let head g = g |> toListn 1 |> List.exactlyOne

    let skip n g =
        loop {
            let! v = g
            let! c = count 0 1
            if c >= n then
                yield v
        }

    // TODO: has "truncate" behaviour
    let take n g =
        loop {
            let! v = g
            let! c = count 0 1
            if c < n then
                yield v
            else
                return Loop.Stop
        }


    // /////////////////////////////////
    // New Things //////////////////////
    // /////////////////////////////////

    let initWith factory =
        fun state ->
            let state = state |> Option.defaultWith factory
            Res.Loop.emit state state
        |> createLoop

    let ofMutable (initialValue: 'a when 'a: struct) =
        fun state ->
            let state = state |> Option.defaultWith (fun () -> ref initialValue)
            Res.Loop.emit (state.contents, fun value -> state.contents <- value) state
        |> createLoop

    // TODO: Combine() with "unit" so that "do for ..." or "if for ..." can be used
    // TODO: No implicit "Zero"
    // TODO: Skip in Reevaluate umbenennen?
    // - Die einzelnen loops könnten für sich neu antriggerbar sein. Somit müsste bei Events nicht immer alles durchlaufen werden
    // - Damit man wieder pure Daten hat, könnte man nicht die HTMLElements direkt im State halten,
    //   sondern ein runtime dict in der app zur Verfügung stellen




[<AutoOpen>]
module TopLevelOperators =
    /// Kleisli operator (fx >> fx)
    let inline (>=>) ([<InlineIfLambda>] f) ([<InlineIfLambda>] g) = Gen.pipeFx g f

    /// Kleisli "pipe" operator (gen >> fx)
    let inline (|=>) f ([<InlineIfLambda>] g) = Gen.pipe g f

    /// Bind operator
    let inline (>>=) m ([<InlineIfLambda>] f) = Gen.bind f m

    let loop = Gen.loop
    let feed = Gen.feed


[<RequireQualifiedAccess>]
module Arithmetic =
    let inline binOpBoth left right f =
        loop {
            let! l = left
            let! r = right
            yield f l r
        }
    
    let inline binOpLeft left right f =
        loop {
            let l = left
            let! r = right
            yield f l r
        }
    
    let inline binOpRight left right f =
        loop {
            let! l = left
            let r = right
            yield f l r
        }


type Gen<'o,'s> with
    // the 'comparison' constraint is a hack to prevent ambiguities in
    // F# operator overload resolution.

    // TODO: document operators and especially ==
    
    static member inline (+) (left: ^a when ^a: comparison, right) = Arithmetic.binOpLeft left right (+)
    static member inline (-) (left: ^a when ^a: comparison, right) = Arithmetic.binOpLeft left right (-)
    static member inline (*) (left: ^a when ^a: comparison, right) = Arithmetic.binOpLeft left right (*)
    static member inline (/) (left: ^a when ^a: comparison, right) = Arithmetic.binOpLeft left right (/)
    static member inline (%) (left: ^a when ^a: comparison, right) = Arithmetic.binOpLeft left right (%)
    static member inline (==) (left: ^a when ^a: comparison, right) = Arithmetic.binOpLeft left right (=)

    static member inline (+) (left, right: ^a when ^a: comparison) = Arithmetic.binOpRight left right (+)
    static member inline (-) (left, right: ^a when ^a: comparison) = Arithmetic.binOpRight left right (-)
    static member inline (*) (left, right: ^a when ^a: comparison) = Arithmetic.binOpRight left right (*)
    static member inline (/) (left, right: ^a when ^a: comparison) = Arithmetic.binOpRight left right (/)
    static member inline (%) (left, right: ^a when ^a: comparison) = Arithmetic.binOpRight left right (%)
    static member inline (==) (left, right: ^a when ^a: comparison) = Arithmetic.binOpRight left right (=)

    static member inline (+) (left, right) = Arithmetic.binOpBoth left right (+)
    static member inline (-) (left, right) = Arithmetic.binOpBoth left right (-)
    static member inline (*) (left, right) = Arithmetic.binOpBoth left right (*)
    static member inline (/) (left, right) = Arithmetic.binOpBoth left right (/)
    static member inline (%) (left, right) = Arithmetic.binOpBoth left right (%)
    static member inline (==) (left, right) = Arithmetic.binOpBoth left right (=)


open System.Runtime.CompilerServices

[<Extension>]
type Extensions() =

    [<Extension>]
    static member GetSlice(inputGen: LoopGen<'o, 's>, inclStartIdx, inclEndIdx) =
        let s = max 0 (defaultArg inclStartIdx 0)
        loop {
            let! i = Gen.count 0 1
            let! value = inputGen
            if i >= s then
                match inclEndIdx with
                | Some e when i > e ->
                    return Loop.Stop
                | _ ->
                    yield value
        }
