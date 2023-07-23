namespace Vide

type Vide<'v,'s,'c> = Vide of ('s option -> IApp -> 'c -> 'v * 's)

and IApp =
    abstract member RequestEvaluation: unit -> unit
    abstract member SuspendEvaluation: unit -> unit
    abstract member ResumeEvaluation: unit -> unit

type NoState = NoState

[<RequireQualifiedAccess>]
module BuilderBricks =
    let bind<'v1,'s1,'v2,'s2,'c>
        (
            Vide m: Vide<'v1,'s1,'c>,
            f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 * 's2,'c>
        =
        Vide <| fun s app ctx ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> Some ms, Some fs
            let mv,ms = m ms app ctx
            let (Vide f) = f mv
            let fv,fs = f fs app ctx
            fv, (ms,fs)

    let return'<'v,'c>(x: 'v)
        : Vide<'v,NoState,'c> 
        =
        Vide <| fun s app ctx -> x,NoState

    let yield'<'v,'s,'c>(v: Vide<'v,'s,'c>) 
        : Vide<'v,'s,'c>
        =
        v

    let zero<'c>() 
        : Vide<unit,NoState,'c>
        =
        Vide <| fun s app ctx -> (),NoState

    let delay<'v,'s,'c>(f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        = 
        f()

    let combine<'v1,'s1,'v2,'s2,'c>
        (
           Vide a: Vide<'v1,'s1,'c>,
           Vide b: Vide<'v2,'s2,'c>
        )
        : Vide<'v2,'s1 * 's2,'c>
        =
        Vide <| fun s app ctx ->
            let sa,sb =
                match s with
                | None -> None,None
                | Some (ms,fs) -> Some ms, Some fs
            let va,sa = a sa app ctx
            let vb,sb = b sb app ctx
            vb, (sa,sb)

    let for'<'a,'v,'s,'c when 'a: comparison>
        (
            input: seq<'a>,
            body: 'a -> Vide<'v,'s,'c>
        ) 
        : Vide<'v list, Map<'a, 's>,'c>
        = 
        Vide <| fun s app ctx ->
            let mutable currMap = s |> Option.defaultValue Map.empty
            let resValues,resStates =
                [ for x in input do
                    let matchingState = currMap |> Map.tryFind x
                    let v,s = 
                        let (Vide v) = body x
                        v matchingState app ctx
                    do currMap <- currMap |> Map.remove x
                    v, (x,s)
                ]
                |> List.unzip
            resValues, (resStates |> Map.ofList)

type VideBuilder() =
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zero()
