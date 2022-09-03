

let inline unwrapTupledState s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

// why we return 's option(!!) -> Because of else branch / zero
type Gen<'v,'s,'r> =
    Gen of ('s option -> 'r -> 'v * 's option) with

    member inline _.Bind(
        Gen m: Gen<'v1,'s1,'r>,
        f: 'v1 -> Gen<'v2,'s2,'r>)
        : Gen<'v2,'s1 option * 's2 option,'r>
        =
        Gen <| fun s r ->
            let ms,fs = unwrapTupledState s
            let mv,ms = m ms r
            let (Gen fgen) = f mv
            let fv,fs = fgen fs r
            fv, Some (ms,fs)
    
    member inline _.Return(x) =
        Gen <| fun s r -> x,None

    member inline _.Yield(
        x: Gen<'v,'s,'r>)
        : Gen<'v,'s,'r>
        =
        x

    member inline _.Zero()
        : Gen<unit,'s,'r>
        =
        Gen <| fun s r ->  (), None

    member inline _.Delay(
        f: unit -> Gen<'v,'s,'r>)
        : Gen<'v,'s,'r>
        =
        f()

    member inline _.Combine(
        Gen a: Gen<'elem,'s1,'r>,
        Gen b: Gen<'elem,'s2,'r>)
        : Gen<unit,'s1 option * 's2 option,'r>
        =
        Gen <| fun s r ->
            let sa,sb = unwrapTupledState s
            let va,sa = a sa r
            let vb,sb = b sb r
            (), Some (sa,sb)

    member inline _.For(
        sequence: seq<'a>,
        body: 'a -> Gen<unit,'s,'r>)
        : Gen<unit,'s option list,'r>
        =
        Gen <| fun s r ->
            let s = s |> Option.defaultValue []
            let res = 
                [ for i,x in sequence |> Seq.mapi (fun i x -> i,x) do
                    let (Gen f) = body x
                    let fres = f (s |> List.tryItem i |> Option.flatten) r
                    fres
                ]
            (), Some (res |> List.map snd)


let toStateMachine initialState app (Gen g) =
    let mutable state = initialState
    let eval () =
        let _,newState = g state app
        state <- newState
    eval

let preserve x =
    Gen <| fun s r ->
        let s = s |> Option.defaultValue x
        s, Some s

let mut x =
    Gen <| fun s r ->
        let s = s |> Option.defaultWith (fun () -> ref x)
        s, Some s

