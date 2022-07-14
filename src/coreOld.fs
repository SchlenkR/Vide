
namespace LocSta

open System

// TODO: struct tuples

type Gen<'o,'s,'r> = Gen of ('s option -> 'r -> ('o * 's))

module Gen =
    
    /// Bind with transparent (nested) state typing.
    let inline bind 
        (Gen m: Gen<'o1,'s1,'r>)
        ([<InlineIfLambda>] f: 'o1 -> Gen<'o2,'s2,'r>)
        : Gen<'o2, 's1 * 's2, 'r>
        =
        fun mfState r ->
            // unpack the previous state (may be None or Some)
            let mState,fState =
                match mfState with
                | None -> None,None
                | Some (mState,fState) -> Some mState, Some fState

            // The result of m is made up of an actual value and a state that
            // has to be "recorded" by packing it together with the state of the
            // next gen.
            let mOut,mState' = m mState r

            // Continue evaluating the computation:
            // passing the actual output value of m to the rest of the computation
            // gives us access to the next gen in the computation:
            let (Gen fgen) = f mOut

            // Evaluate the next gen and build up the result of this bind function
            // as a gen, so that it can be used as a bindable element itself -
            // but this time with state of 2 gens packed together.
            let fOut,fState' = fgen fState r

            let resultingState = mState', fState'
            fOut, resultingState
        |> Gen

    /// Bind with hidden (boxed) state typing.
    module BindBoxed =
        type BoxedState = { stateType: Type; state: obj }
        type CombinedBoxedState = { mState: BoxedState; fState: BoxedState }
        
        let internal unboxState<'t> state =
            match state with
            | None -> None
            | Some x -> Some (x.state :?> 't)

        let inline bind
            (Gen m: Gen<'o1,'s1,'r>)
            ([<InlineIfLambda>] f: 'o1 -> Gen<'o2,'s2,'r>)
            : Gen<'o2, CombinedBoxedState, 'r>
            =
            fun mfState r ->
                let mState,fState =
                    match mfState with
                    | None -> None,None
                    | Some mfState -> Some mfState.mState, Some mfState.fState
                let mOut,mState' = m (unboxState mState) r
                let (Gen fgen) = f mOut
                let fOut,fState' = fgen (unboxState fState) r
                let resultingState =
                    { mState = { stateType = mState'.GetType(); state = mState' }
                      fState = { stateType = fState'.GetType(); state = fState' }
                    }
                fOut, resultingState
            |> Gen

    let inline ofValue x = Gen (fun s r -> x,())

    type GenBuilder() =
        member inline _.Bind(m, [<InlineIfLambda>] f) = bind m f
        member _.Return(x) = ofValue x
        member _.ReturnFrom(x) : Gen<_,_,_> = x

    let loop = GenBuilder()

    let inline map proj (Gen g) = 
        Gen <| fun s r ->
            let o,s = g s r in proj o, s

    let preserve factory = 
        Gen <| fun s r ->
            let state = s |> Option.defaultWith factory
            state,state

    let ofMutable initialValue = 
        Gen <| fun s r ->
            let refCell = s |> Option.defaultWith (fun () -> ref initialValue)
            let setter = fun value -> refCell.contents <- value
            (refCell.contents, setter), refCell

    let inline toEvaluable (Gen g) =
        let mutable state = None
        fun r ->
            let fOut,fState = g state r
            state <- Some fState
            fOut

    // TODO: use toEvaluable
    let inline toSeq r g =
        let evaluable = toEvaluable g
        seq { while true do yield evaluable r  }

    let inline withState (Gen g) =
        Gen <| fun s r ->
            let o,s = g s r in (o,s),s
