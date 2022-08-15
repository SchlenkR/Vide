
open System

type AppGen<'v,'s> = Gen of ('s option -> 'v * 's)
type NoState = NoState

type Builder() =
    member inline _.Bind(
        Gen m: AppGen<'v1,'s1>,
        f: 'v1 -> AppGen<'v2,'s2>)
        : AppGen<'v2,'s1 * 's2>
        =
        printfn "Bind"
        Gen <| fun mfState ->
            let ms,fs =
                match mfState with
                | None -> None,None
                | Some (ms,fs) -> Some ms, Some fs
            let mv,ms = m ms
            let (Gen fgen) = f mv
            let fv,fs = fgen fs
            fv, (ms,fs)

    // member _.Yield(
    //     x: AppGen<'elem,'s>)
    //     : AppGen<'node, 's>
    //     =
    //     printfn "Yield"
    //     failwith ""
    // member _.Delay(
    //     f: unit -> AppGen<'elem,'s>)
    //     : AppGen<'node,'s>
    //     =
    //     failwith ""
    // member _.Combine(
    //     a: AppGen<'elem,'s1>,
    //     b: AppGen<'elem,'s2>)
    //     : AppGen<'elem,'s3>
    //     =
    //     printfn "Combine"
    //     failwith ""

    member inline _.For(
        sequence: seq<'a>,
        body: 'a -> AppGen<'v,'s>)
        : AppGen<'v list,'s list>
        =
        printfn "For"
        Gen <| fun s ->
            let s = s |> Option.defaultValue []
            let res = 
                [ for i,x in sequence |> Seq.mapi (fun i x -> i,x) do
                    let (Gen f) = body x
                    let fres = f (s |> List.tryItem i)
                    fres
                ]
            res |> List.map fst, res |> List.map snd

    member inline _.Zero()
        : AppGen<'v,'s>
        =
        printfn "Zero"
        Gen <| fun _ -> failwith ""
    // member _.Run(
    //     children: AppGen<'s1>)
    //     : 'ret
    //     =
    //     printfn "Run"
    //     failwith ""

let pov = Builder()
let htmlElem x = Gen <| fun s -> (),x
let state x = Gen <| fun s -> x,()

let inline e() =
    pov {
        do! htmlElem 1
        do! htmlElem "2"
        do! htmlElem 3
        
        for x in 0..10 do
            do! htmlElem "4"

            if x % 2 = 0 then
                do! htmlElem (5 + x )
    }

let (Gen x: AppGen<unit,'s>) = e()
x None



let d() =
    pov {
        do! htmlElem 1
        do! htmlElem "2"
        do! htmlElem 3
        for x in 0..10 do
            do! htmlElem "4"
            do! htmlElem (5 + x)
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
