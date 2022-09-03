
[<AutoOpen>]
module Vide.Core

let inline internal separateStatePair s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

let inline internal log name =
    printfn $"        Exex:   {name}"

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

type VideBuilder() =
    member inline _.Bind
        (
            Vide m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide <| fun s c ->
            let ms,fs = separateStatePair s
            let mv,ms = m ms c
            let (Vide v) = f mv
            let vres,fs = v fs c
            vres, Some (ms,fs)
    member inline _.Return(x) =
        Vide <| fun s c -> x,None
    //member inline _.Yield
    //    (x: Vide<'v,'s,'c>)
    //    : Vide<'v,'s,'c>
    //    =
    //    x
    member inline _.Zero()
        : Vide<unit,'s,'c>
        =
        Vide <| fun s c ->  (), None
    member inline _.Delay
        (f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        f()
    member inline _.Combine
        (
            Vide a: Vide<'elem,'s1,'c>,
            Vide b: Vide<'elem,'s2,'c>
        ) : Vide<unit,'s1 option * 's2 option,'c>
        =
        Vide <| fun s c ->
            let sa,sb = separateStatePair s
            let va,sa = a sa c
            let vb,sb = b sb c
            (), Some (sa,sb)
    member inline _.For
        (
            sequence: seq<'a>,
            body: 'a -> Vide<unit,'s,'c>
        ) : Vide<unit, Map<'a, 's option>,'c>
        =
        Vide <| fun s c ->
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
        ctx,
        vide: Vide<'v,'s,'c>,
        onEvaluated: 'v -> 's option -> unit
    )
    =
    let (Vide vide) = vide
    let mutable state = initialState
    member _.CurrentState with get() = state
    member _.Eval() =
        let value,newState = vide state ctx
        state <- newState
        onEvaluated value state
