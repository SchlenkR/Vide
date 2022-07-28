
type AppGen<'v,'s> = Gen of ('s option -> 'v * 's option)

let inline unwrapTupledState s =
    match s with
    | None -> None,None
    | Some (ms,fs) -> ms,fs

type Builder() =

    member inline _.Bind(
        Gen m: AppGen<'v1,'s1>,
        f: 'v1 -> AppGen<'v2,'s2>)
        : AppGen<'v2,'s1 option * 's2 option>
        =
        printfn "Bind"
        Gen <| fun s ->
            let ms,fs = unwrapTupledState s
            let mv,ms = m ms
            let (Gen fgen) = f mv
            let fv,fs = fgen fs
            fv, Some (ms,fs)
    
    member inline _.Return(x) =
        Gen <| fun s -> x,None

    member inline _.Yield(
        x: AppGen<'v,'s>)
        : AppGen<'v, 's>
        =
        printfn "Yield"
        x

    member inline _.Delay(
        f: unit -> AppGen<'v,'s>)
        : AppGen<'v,'s>
        =
        printfn "Delay"
        f()

    member inline this.Combine(
        Gen a: AppGen<'elem,'s1>,
        Gen b: AppGen<'elem,'s2>)
        : AppGen<unit,'s1 option * 's2 option>
        =
        printfn "Combine"
        Gen <| fun s ->
            let sa,sb = unwrapTupledState s
            let va,sa = a sa
            let vb,sb = b sb
            (), Some (sa,sb)


    member inline _.For(
        sequence: seq<'a>,
        body: 'a -> AppGen<unit,'s>)
        : AppGen<unit,'s option list>
        =
        printfn "For"
        Gen <| fun s ->
            let s = s |> Option.defaultValue []
            let res = 
                [ for i,x in sequence |> Seq.mapi (fun i x -> i,x) do
                    let (Gen f) = body x
                    let fres = f (s |> List.tryItem i |> Option.flatten)
                    fres
                ]
            // res |> List.map fst, res |> List.map snd
            (), Some (res |> List.map snd)

    member inline _.Zero()
        : AppGen<unit,'s>
        =
        printfn "Zero"
        Gen <| fun _ ->
            // We have to be generic because of implicit "else" branch.
            // TODO: We should not lose that info; so value and state must be
            // wrapped in all places with (Zero | Value of 'a)
            (), None

    // member _.Run(
    //     children: AppGen<'s1>)
    //     : 'ret
    //     =
    //     printfn "Run"
    //     failwith ""

let pov = Builder()
let eval (Gen g) state = g state

let globalResult = ResizeArray<obj>()

let htmlElem x =
    Gen <| fun s ->
        match s with
        | None ->
            printfn $"adding state: {x}"
            do globalResult.Add x
        | Some s ->
            printfn $"using state: {s}"
        (), Some x

let preserve x =
    Gen <| fun s ->
        let s = s |> Option.defaultValue x
        s, Some s

let mut x =
    Gen <| fun s ->
        let s = s |> Option.defaultWith (fun () -> ref x)
        s, Some s



let inline e() =
    pov {
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


let _,s1 = eval (e()) None
let _,s2 = eval (e()) s1




let d() =
    pov {
        htmlElem 1
        htmlElem "2"
        htmlElem 3
        for x in 0..10 do
            htmlElem "4"
            htmlElem (5 + x)
            // Zero
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
