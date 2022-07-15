
// module Test

// type Builder() =
//     member inline this.Yield(x) = [x]
//     member inline _.Delay(f: unit -> _) = f
//     member _.Combine(a, b) = List.append a (b())
//     member _.Zero() = []
//     member _.Run(children) = children()
//     member inline _.For(inputList: seq<'a>, body: 'a -> list<'b>) : list<'b> =
//         [ for x in inputList do yield! body x ]
// let test = Builder()

// let res = 
//     test { 
//         for x in 0..10 do
//             string x
//     }








// #if INTERACTIVE
// #else
// module Test
// #endif


// type Gen<'o> = { value: 'o }
// type RTGen = { message: string }

// let toRTGen x = { message = string x.value }
// let bind (m: Gen<_>) (f: _ -> Gen<_>) : Gen<_> = f m.value

// // type Delayed = 

// type Builder() =
//     member _.Bind(m: Gen<'o>, f: 'o -> RTGen) : RTGen =
//         bind m (fun v ->
//             let rtgen = f v
//             { value = rtgen.message }
//         )
//         |> toRTGen
//     member _.Return(x: RTGen) = x
//     // member _.Delay(f) = f
//     // member _.Combine(a, b) = a :: b
//     // member _.Zero() = []
//     // member _.Run(children: RTGen list) = children

// let test = Builder()

// let res = 
//     test { 
//         let! x = { value = 100 }
//         let! y = { value = 200 }
//         return { message = $"First value is: {x} and {y}"}
    
//         // let! z = { value = "Hello World" }
//         // return { message = $"Second value is: {z}"}
//     }




#if INTERACTIVE
#else
module Test
#endif

open System

type Gen<'o,'s> = { value: 'o; state: 's }

type RTState = { stateTypeName: string; state: obj }

let toRTState (x: 'a) =
    let rec getTypeName (t: Type) =
        match t.GenericTypeArguments with
        | [| |] -> t.Name
        | args ->
            let args = args |> Array.map getTypeName |> String.concat ", "
            $"{t.Name}<{args}>"
    { stateTypeName = getTypeName typeof<'a>; state = x :> obj }

let bind (m: Gen<'a,'s1>) (f: 'a -> Gen<'b,'s2>) : Gen<'b, 's1 * 's2> =
    let fres = f m.value
    { value = fres.value; state = m.state,fres.state }

let ofValue v = { value = v; state = () }

type YieldedOrCombined<'a> = YieldedOrCombined of 'a list
type Delayed<'a> = Delayed of 'a list

type Builder() =
    member inline _.Bind(m: Gen<'a,'s1>, f: 'a -> Gen<'b,'s2>) : Gen<'b,RTState> =
        printfn $"BIND     -  m.value = {m.value}"
        let res = bind m f
        {
            value = res.value
            state = toRTState res.state
        }
    member _.Yield(x: Gen<_,_>) =
        printfn $"YIELD    -  x.value = {x.value}"
        { value = YieldedOrCombined [x.value]; state = x.state }
    member _.Delay(f: unit -> Gen<YieldedOrCombined<_>, _>) : Gen<Delayed<_>,_> =
        let fres = f()
        let (YieldedOrCombined fvalue) = fres.value
        printfn $"DELAY    -  f() = {fvalue}"
        { value = Delayed fvalue; state = fres.state }
    member _.Combine(a: Gen<YieldedOrCombined<'a>, 's>, b: Gen<Delayed<'a>, RTState>) =
        printfn $"COMBINE  -  a.value = {a.value}  -  b.value = {b.value}"
        let (YieldedOrCombined avalues) = a.value
        let (Delayed bvalues) = b.value
        {
            value = YieldedOrCombined (avalues @ bvalues)
            state = toRTState (a.state, b.state)
        }
    // member _.Zero() = 
    //     printfn $"ZERO"
    //     []
    // member _.Run(x) =
    //     printfn $"RUN      -   {x}"
    //     x

let test = Builder()

let x =
    test {
        let! a = { value = 100; state = "a" }
        let! b = { value = 200; state = 44.2 }
        { value = a + b; state = 20UL }
        
        let! c = { value = 33; state = "c" }
        let! d = { value = 66; state = 44.1 }
        { value = c + d; state = 10.0 }

        { value = -77; state = 20.0 }

        let! e = { value = -2; state = [909090] }
        let! f = { value = -3; state = (0.1, 0.2, 0.3) }
        { value = e + f; state = ("Hello", "World") }
    }

    // |> ignore
