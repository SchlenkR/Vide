
[<AutoOpen>]
module Fiu.Core

// why we return 's option(!!) -> Because of else branch / zero
type Gen<'v,'s,'c> = Gen of ('s option -> 'c -> 'v * 's option)

let inline internal unwrapTupledState s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

let inline internal printMethod name =
    // printfn $"        Exex:   {name}"
    ()

type FiuBuilder<'fs1,'fs2,'c>(
    run: Gen<unit,'fs1,'c> -> Gen<unit,'fs2,'c>)
    =
    
    member inline _.Bind(
        Gen m: Gen<'v1,'s1,'c>,
        f: 'v1 -> Gen<'v2,'s2,'c>)
        : Gen<'v2,'s1 option * 's2 option,'c>
        =
        printMethod "Bind"
        Gen <| fun s c ->
            let ms,fs = unwrapTupledState s
            let mv,ms = m ms c
            let (Gen fgen) = f mv
            let fv,fs = fgen fs c
            fv, Some (ms,fs)
    
    member inline _.Return(x) =
        printMethod "Return"
        Gen <| fun s c -> x,None

    member inline _.Yield(
        x: Gen<'v,'s,'c>)
        : Gen<'v,'s,'c>
        =
        printMethod "Yield"
        x

    member inline _.Zero()
        : Gen<unit,'s,'c>
        =
        printMethod "Zero"
        Gen <| fun s c ->  (), None

    member inline _.Delay(
        f: unit -> Gen<'v,'s,'c>)
        : Gen<'v,'s,'c>
        =
        printMethod "Delay"
        f()

    member inline _.Combine(
        Gen a: Gen<'elem,'s1,'c>,
        Gen b: Gen<'elem,'s2,'c>)
        : Gen<unit,'s1 option * 's2 option,'c>
        =
        printMethod "Combine"
        Gen <| fun s c ->
            let sa,sb = unwrapTupledState s
            let va,sa = a sa c
            let vb,sb = b sb c
            (), Some (sa,sb)

    member inline _.For(
        sequence: seq<'a>,
        body: 'a -> Gen<unit,'s,'c>)
        : Gen<unit,'s option list,'c>
        =
        printMethod "For"
        Gen <| fun s c ->
            let s = s |> Option.defaultValue []
            let res = 
                [ for i,x in sequence |> Seq.mapi (fun i x -> i,x) do
                    let (Gen f) = body x
                    let fres = f (s |> List.tryItem i |> Option.flatten) c
                    fres
                ]
            (), Some (res |> List.map snd)

    member this.Run(
        childGen: Gen<unit,'fs1,'c>)
        : Gen<unit,'fs2,'c>
        =
        printMethod "Run"
        run childGen

let toStateMachine initialState ctx (Gen g) =
    let mutable state = initialState
    let eval () =
        let _,newState = g state ctx
        state <- newState
    eval

let preserve x =
    Gen <| fun s c ->
        let s = s |> Option.defaultValue x
        s, Some s

let mut x =
    Gen <| fun s c ->
        let s = s |> Option.defaultWith (fun () -> ref x)
        s, Some s


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

    type FiuBuilder<'fs1,'fs2,'c> with
        member inline _.Yield(
            x: FiuBuilder<'s, HtmlElement option * unit option, 'c>)
            : Gen<unit, HtmlElement option * unit option, 'c>
            =
            printMethod "Yield (FiuBuilder)"
            x { () }

    let fiu<'s> = FiuBuilder<'s,'s,Context>(id)

    // HtmlElement muss einen Builder zurückgeben,
    // der bei Run() selbst zu einem Gen wird
    let inline htmlElem data =
        let run (Gen childGen) =
            Gen <| fun s (r: Context) ->
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
                let cv,cs = childGen cs ctx
                (), Some (Some element, cs)
        FiuBuilder(run)

    let dummyCtx = createContext { data = "ROOT" }



    let h =
        fiu {
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
        fiu {
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
        fiu {
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
        fiu {
            htmlElem "a"
        }

    let componentUser =
        fiu {
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
