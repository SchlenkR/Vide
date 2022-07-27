
open System

type AppGen<'s> = Gen of ('s option -> 's)
type NoState = NoState

type Builder() =
    member inline _.Bind(
        Gen m: AppGen<'s1>,
        f: unit -> AppGen<'s2>)
        : AppGen<'s1 * 's2>
        =
        printfn "Bind"
        Gen <| fun mfState ->
            let ms,fs =
                match mfState with
                | None -> None,None
                | Some (ms,fs) -> Some ms, Some fs
            let mres = m ms
            let (Gen fgen) = f ()
            let fres = fgen fs
            mres,fres
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
        body: 'a -> AppGen<'s>)
        : AppGen<'s list>
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
            res

    member inline _.Zero()
        : AppGen<'a>
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
let state x = Gen <| fun s -> x

type IfElseState<'s> =
    | IfBranch of AppGen<'s>
    | ElseBranch of AppGen<'s>

let elseNothing<'s> : AppGen<IfElseState<'s>> =
    Gen <| fun s -> failwith "nothing"

let e() =
    pov {
        do! state 1
        do! state "2"
        do! state 3
        for x in 0..10 do
            do! state "4"

            if x % 2 = 0 then
                do! state (5 + x)
    }

let (Gen x) = e() in x None



let d() =
    pov {
        do! state 1
        do! state "2"
        do! state 3
        for x in 0..10 do
            do! state "4"
            do! state (5 + x)
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
