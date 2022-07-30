
type HtmlElement = { data: obj }

type App =
    { addElement: HtmlElement -> unit
      keepElement: HtmlElement -> unit
    }

// why we return 's option(!!) -> Because of else branch / zero
type AppGen<'v,'s> = Gen of ('s option -> App -> 'v * 's option)

let inline unwrapTupledState s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

let inline printMethod name =
    // printfn $"        Exex:   {name}"
    ()

type PovBuilder<'finState1,'finState2>(
    run: AppGen<unit,'finState1> -> AppGen<unit,'finState2>)
    =
    
    member inline _.Bind(
        Gen m: AppGen<'v1,'s1>,
        f: 'v1 -> AppGen<'v2,'s2>)
        : AppGen<'v2,'s1 option * 's2 option>
        =
        printMethod "Bind"
        Gen <| fun s r ->
            let ms,fs = unwrapTupledState s
            let mv,ms = m ms r
            let (Gen fgen) = f mv
            let fv,fs = fgen fs r
            fv, Some (ms,fs)
    
    member inline _.Return(x) =
        printMethod "Return"
        Gen <| fun s r -> x,None

    member inline _.Yield(
        x: AppGen<'v,'s>)
        : AppGen<'v, 's>
        =
        printMethod "Yield"
        x

    member inline _.Zero()
        : AppGen<unit,'s>
        =
        printMethod "Zero"
        Gen <| fun s r ->  (), None

    member inline _.Delay(
        f: unit -> AppGen<'v,'s>)
        : AppGen<'v,'s>
        =
        printMethod "Delay"
        f()

    member inline this.Combine(
        Gen a: AppGen<'elem,'s1>,
        Gen b: AppGen<'elem,'s2>)
        : AppGen<unit,'s1 option * 's2 option>
        =
        printMethod "Combine"
        Gen <| fun s r ->
            let sa,sb = unwrapTupledState s
            let va,sa = a sa r
            let vb,sb = b sb r
            (), Some (sa,sb)

    member inline _.For(
        sequence: seq<'a>,
        body: 'a -> AppGen<unit,'s>)
        : AppGen<unit,'s option list>
        =
        printMethod "For"
        Gen <| fun s r ->
            let s = s |> Option.defaultValue []
            let res = 
                [ for i,x in sequence |> Seq.mapi (fun i x -> i,x) do
                    let (Gen f) = body x
                    let fres = f (s |> List.tryItem i |> Option.flatten) r
                    fres
                ]
            (), Some (res |> List.map snd)

    member this.Run(
        childGen: AppGen<unit,'finState1>)
        : AppGen<unit,'finState2>
        =
        printMethod "Run"
        run childGen

let pov<'s> = PovBuilder<'s,'s>(id)

let eval (Gen g) state = g state



let createApp parentElement =
    {
        addElement = fun childElement -> printfn $"Adding child: {parentElement.data} -> {childElement.data}"
        keepElement = fun childElement -> printfn $"Keeping child: {parentElement.data} -> {childElement.data}"
    }

// HtmlElement muss einen Builder zurückgeben,
// der bei Run() selbst zu einem Gen wird
let inline htmlElem data =
    let run (Gen childGen) =
        Gen <| fun s r ->
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
            let app = createApp element
            let cv,cs = childGen cs app
            (), Some (Some element, cs)
    PovBuilder(run)

let preserve x =
    Gen <| fun s r ->
        let s = s |> Option.defaultValue x
        s, Some s

let mut x =
    Gen <| fun s r ->
        let s = s |> Option.defaultWith (fun () -> ref x)
        s, Some s

let dummyApp = createApp { data = "ROOT" }

let inline empty (builder: PovBuilder<'s, HtmlElement option * unit option>) = builder { () }





let h () =
    pov {
        htmlElem 1 {
            htmlElem "1_1" |> empty
            htmlElem 1.2 |> empty
        }

        // htmlElem "2" |> empty
        // htmlElem 3 |> empty
        // for x in 0..10 do
        //     htmlElem "4" |> empty
        //     htmlElem (5 + x) |> empty
        //     // Zero
    }

let _,sh1 = eval (h()) None dummyApp
let _,sh2 = eval (h()) sh1 dummyApp




let d() =
    pov {
        htmlElem 1 |> empty
        htmlElem "2" |> empty
        htmlElem 3 |> empty
        for x in 0..10 do
            htmlElem "4" |> empty
            htmlElem (5 + x) |> empty
            // Zero
    }

let _,sd1 = eval (d()) None dummyApp
let _,sd2 = eval (d()) sd1 dummyApp


let inline e() =
    pov {
        let! runCount = mut 0
        printfn $"RunCount = {runCount.contents}"
        runCount.contents <- runCount.contents + 1

        htmlElem 1 |> empty
        htmlElem "2" |> empty
        htmlElem 3 |> empty
        
        for x in 0..10 do
            let! forCount = mut 0
            printfn $"ForCount = {forCount.contents}"
            forCount.contents <- forCount.contents + 1
            
            htmlElem "4" |> empty

            if x % 2 = 0 then
                htmlElem $"      IF -  {5 + x}" |> empty
    }


let _,se1 = eval (e()) None dummyApp
let _,se2 = eval (e()) se1 dummyApp




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
