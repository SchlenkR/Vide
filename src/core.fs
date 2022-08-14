
[<AutoOpen>]
module Vide.Core

// why we return 's option(!!) -> Because of else branch / zero
type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

let inline internal separateStatePair s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

let inline internal log name =
    printfn $"        Exex:   {name}"
    // ()

type VideBuilder<'fs1,'fs2,'c>(
    run: Vide<unit,'fs1,'c> -> Vide<unit,'fs2,'c>)
    =
    member inline _.Bind(
        Vide m: Vide<'v1,'s1,'c>,
        f: 'v1 -> Vide<'v2,'s2,'c>)
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        log "Bind"
        Vide <| fun s c ->
            let ms,fs = separateStatePair s
            let mv,ms = m ms c
            let (Vide v) = f mv
            let vres,fs = v fs c
            vres, Some (ms,fs)
    
    member inline _.Return(x) =
        log "Return"
        Vide <| fun s c -> x,None
    member inline _.Yield(
        x: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        log "Yield"
        x
    member inline _.Zero()
        : Vide<unit,'s,'c>
        =
        log "Zero"
        Vide <| fun s c ->  (), None
    member inline _.Delay(
        f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        log "Delay"
        f()
    member inline _.Combine(
        Vide a: Vide<'elem,'s1,'c>,
        Vide b: Vide<'elem,'s2,'c>)
        : Vide<unit,'s1 option * 's2 option,'c>
        =
        log "Combine"
        Vide <| fun s c ->
            let sa,sb = separateStatePair s
            let va,sa = a sa c
            let vb,sb = b sb c
            (), Some (sa,sb)
    member inline _.For(
        sequence: seq<'a>,
        body: 'a -> Vide<unit,'s,'c>)
        : Vide<unit,'s option list,'c>
        =
        log "For"
        Vide <| fun s c ->
            let s = s |> Option.defaultValue []
            let res = 
                [ for i,x in sequence |> Seq.mapi (fun i x -> i,x) do
                    let (Vide v) = body x
                    let vres = v (s |> List.tryItem i |> Option.flatten) c
                    vres
                ]
            (), Some (res |> List.map snd)
    member _.Run(
        childVide: Vide<unit,'fs1,'c>)
        : Vide<unit,'fs2,'c>
        =
        log "Run"
        run childVide

let preserve x =
    Vide <| fun s c ->
        let s = s |> Option.defaultValue x
        s, Some s

let toStateMachine initialState ctx (Vide vide) =
    let mutable state = initialState
    let eval () =
        let _,newState = vide state ctx
        state <- newState
    eval


(*
module private Html =

    type HtmlElement = { data: obj }

    type Context =
        {
            addElement: HtmlElement -> unit
            keepElement: HtmlElement -> unit
        }

    let createContext parentElement =
        {
            addElement = fun childElement -> printfn $"Adding child: {parentElement.data} -> {childElement.data}"
            keepElement = fun childElement -> printfn $"Keeping child: {parentElement.data} -> {childElement.data}"
        }

    type VideBuilder<'fs1,'fs2,'c> with
        member inline _.Yield(
            x: VideBuilder<'s, HtmlElement option * unit option, 'c>)
            : Vide<unit, HtmlElement option * unit option, 'c>
            =
            printMethod "Yield (VideBuilder)"
            x { () }

    let vide<'s> = VideBuilder<'s,'s,Context>(id)

    let inline htmlElem data 
        : VideBuilder<'a, option<HtmlElement> * option<'a>,Context> 
        =
        let run (Vide childVide) =
            Vide <| fun s (r: Context) ->
                let s,cs = unwrapTupledState s
                let element =
                    match s with
                    | None ->
                        let element = { data = data }
                        do r.addElement element
                        element
                    | Some element ->
                        do r.keepElement element
                        element
                let ctx = createContext element
                let cv,cs = childVide cs ctx
                (), Some (Some element, cs)
        VideBuilder(run)

    let dummyCtx = createContext { data = "ROOT" }



    let h =
        vide {
            htmlElem 1 {
                htmlElem "1_1"
                
                let! forCount = mut 0
                do forCount.Value <- forCount.Value + 1
                printfn $"----------- forCount = {forCount.Value}"

                htmlElem forCount.Value
            }

            // htmlElem "2"
            // htmlElem 3
            // for x in 0..10 do
            //     htmlElem "4"
            //     htmlElem (5 + x)
            //     // Zero
        }

    let evalH = h |> toStateMachine None dummyCtx
    evalH()



    let d =
        vide {
            htmlElem 1
            htmlElem "2"
            htmlElem 3
            for x in 0..10 do
                htmlElem "4"
                htmlElem (5 + x)
                // Zero
        }

    let evalD = d |> toStateMachine None dummyCtx
    evalD()


    let e =
        vide {
            let! runCount = mut 0
            printfn $"RunCount = {runCount.contents}"
            runCount.contents <- runCount.contents + 1

            htmlElem 1
            htmlElem "2"
            htmlElem 3
            
            for x in 0..10 do
                let! forCount = mut 0
                printfn $"ForCount = {forCount.contents}"
                forCount.contents <- forCount.contents + 1
                
                htmlElem "4"

                if x % 2 = 0 then
                    htmlElem $"      IF -  {5 + x}"
        }


    let evalE = e |> toStateMachine None dummyCtx
    evalE()




    let myComponent =
        vide {
            htmlElem "a"
        }

    let componentUser =
        vide {
            myComponent
        }

    // let (Gen x) = d() in x None


    // let a() =
    //     test {
    //         let! a = { value = 100; state = "a" }
    //         let! b = { value = 200; state = 44.2 }
    //         { value = a + b; state = 20UL }
            
    //         let! c = { value = 33; state = "c" }
    //         let! d = { value = 66; state = 44.1 }
    //         { value = c + d; state = 10.0 }

    //         { value = -77; state = 20.0 }
    //         for i in 0..3 do
    //             { value = -77; state = 20.0 }

    //         let! e = { value = -2; state = [909090] }
    //         let! f = { value = -3; state = (0.1, 0.2, 0.3) }
    //         for i in 0..3 do
    //             { value = e + f + i; state = ("Hello", "World") }
    //             { value = e + f + i; state = ("Hello", "World") }
    //             { value = e + f + i; state = ("Hello", "World") }

    //         { value = e + f; state = ("Hello", "World") }
    //     }


    // let b() =
    //     test {
    //         let! a = { value = 100; state = "a" }
    //         let! b = { value = 200; state = 44.2 }
    //         { value = a + b; state = 20UL }
            
    //         let! c = { value = 33; state = "c" }
    //         let! d = { value = 66; state = 44.1 }
    //         { value = c + d; state = 10.0 }
    //         { value = -77; state = 20.0 }
    //     }


    // let c() =
    //     test {
    //         { value = 33; state = 20UL }
    //         { value = -77; state = 20.0 }
    //     }


    // let e() =
    //     test {
    //         if true then
    //             { value = -77; state = 20.0 }
    //     }

*)