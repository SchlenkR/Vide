
[<AutoOpen>]
module Vide.Core

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

module Debug =
    let mutable printBuilderMethodInvocations = false
    let print s = if printBuilderMethodInvocations then printfn "%s" s

let inline internal separateStatePair s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

let inline internal log name =
    printfn $"        Exex:   {name}"

type VideBuilder() =
    member inline _.Bind
        (
            Vide m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s c ->
            Debug.print "BIND"
            let ms,fs = separateStatePair s
            let mv,ms = m ms c
            let (Vide v) = f mv
            let vres,fs = v fs c
            vres, Some (ms,fs)
    member inline _.Return(x) =
        Vide <| fun s c ->
            Debug.print "RETURN"
            x,None
    member inline _.Zero()
        : Vide<unit,'s,'c>
        =
        Vide <| fun s c ->
            Debug.print "ZERO"
            (), None
    member inline _.Delay
        (f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        Debug.print "DELAY"
        f()
    member inline _.Combine
        (
            Vide a: Vide<'v1,'s1,'c>,
            Vide b: Vide<'v2,'s2,'c>
        ) : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s c ->
            Debug.print "COMBINE"
            let sa,sb = separateStatePair s
            let va,sa = a sa c
            let vb,sb = b sb c
            vb, Some (sa,sb)
    member inline _.For
        (
            sequence: seq<'a>,
            body: 'a -> Vide<unit,'s,'c>
        ) : Vide<unit, Map<'a, 's option>,'c>
        = 
        Vide <| fun s c ->
            Debug.print "FOR"
            let mutable currMap = s |> Option.defaultValue Map.empty
            let res =
                [ for x in sequence do
                    let (Vide v) = body x
                    let matchingState = currMap |> Map.tryFind x |> Option.flatten
                    let _,vs = v matchingState c
                    do currMap <- currMap |> Map.remove x
                    x,vs
                ]
                |> Map.ofList
            (), Some res

let preserve x =
    Vide <| fun s c ->
        let s = s |> Option.defaultValue x
        s, Some s

// TODO: Think about which function is "global" and module-bound
let map (proj: 'v1 -> 'v2) (Vide v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
    Vide <| fun s c ->
        let v,s = v s c
        proj v, s

type VideMachine<'v,'s,'c>
    (
        initialState,
        controller,
        vide: Vide<'v,'s,'c>,
        onEvaluated: 'v -> 's option -> unit
    )
    =
    let (Vide vide) = vide
    let mutable state = initialState
    member _.CurrentState with get() = state
    member _.EvaluateView() =
        let value,newState = vide state controller
        state <- newState
        onEvaluated value state

[<AbstractClass>]
type ControllerBase(evaluateView: unit -> unit) =
    member val EvaluateView = evaluateView with get,set

module Mutable =
    type MutableValue<'a>(init: 'a) =
        let mutable state = init
        member val EvaluateView = (fun () -> ()) with get,set
        member this.Set(value) = state <- value; this.EvaluateView()
        member this.Value
            with get() = state
            and set(value) = this.Set(value)

    let inline change op (mutVal: MutableValue<_>) x =
        mutVal.Value <- op mutVal.Value x
    
    let ofValue x =
        Vide <| fun s (c: #ControllerBase) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x))
            do s.EvaluateView <- c.EvaluateView
            s, Some s
    
    //let list x =
    //    Vide <| fun s (c: Context) ->
    //        let s = s |> Option.defaultWith (fun () -> MutableValue(x))
    //        do s.EvaluateView <- c.evaluateView
    //        s, Some s

let inline ( += ) mutVal x = Mutable.change (+) mutVal x
let inline ( -= ) mutVal x = Mutable.change (-) mutVal x
let inline ( *= ) mutVal x = Mutable.change (*) mutVal x
let inline ( /= ) mutVal x = Mutable.change (/) mutVal x
let inline ( := ) (mutVal: Mutable.MutableValue<_>) x = mutVal.Value <- x

let vide = VideBuilder()
