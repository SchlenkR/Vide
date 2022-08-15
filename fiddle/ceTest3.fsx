open System

type Gen<'v,'s,'r> = { value: 'v; state: 's }
type NoState = NoState
type App = App

type AppGen<'v,'s> = Gen<'v,'s,App>
type BoxedState = BoxedState of obj
type RTAppGen<'v> = { stateType: Type; appGen: AppGen<'v,BoxedState> }

type YieldedOrCombined<'a> = YieldedOrCombined of 'a list
type Delayed<'a> = Delayed of 'a list

let bind (m: AppGen<'a,'s1>) (f: 'a -> AppGen<'b,'s2>) : AppGen<'b,'s1*'s2> =
    let fres = f m.value
    { value = fres.value
      state = m.state,fres.state }


let runBoxedState (BoxedState s) = s
let toRTAppGen (x: AppGen<'v,'s>) =
    { stateType = typeof<'s>
      appGen = { value = x.value; state = BoxedState x.state } }
let inline unboxAppGen<'v,'s> (x: RTAppGen<'v>) : AppGen<'v,'s> =
    { value = x.appGen.value
      state = runBoxedState x.appGen.state :?> 's }

let ofValue v : AppGen<_,_> = { value = v; state = NoState }

// TODO: Could it be that we neet "toRTAppGen" only in bind?
type Builder() =
    member inline _.Bind(
        m: AppGen<'v1,'s>,
        f: 'v1 -> RTAppGen<'v2>)
        : RTAppGen<'v2>
        =
        printfn $"BIND     -  m.value = {m.value}"
        let fres = bind m f
        { value = fres.value
          state = m.state, fres.state }
        |> toRTAppGen
    member _.Yield(
        x: AppGen<'elem,'s>)
        : RTAppGen<YieldedOrCombined<'node>>
        =
        failwith ""
        // printfn $"YIELD    -  x.value = {x.value}"
        // { value = YieldedOrCombined [x.value]
        //   state = x.state }
        // |> toRTAppGen
    member _.Delay(
        f: unit -> RTAppGen<YieldedOrCombined<'elem>>)
        : RTAppGen<Delayed<'node>>
        =
        failwith ""
        // let fres = f()
        // let (YieldedOrCombined fvalue) = fres.appGen.value
        // printfn $"DELAY    -  f() = {fvalue}"
        // { value = Delayed fvalue
        //   state = fres.appGen.state }
        // |> toRTAppGen
    member _.Combine(
        a: RTAppGen<YieldedOrCombined<'elem>>,
        b: RTAppGen<Delayed<'elem>>)
        : RTAppGen<YieldedOrCombined<'elem>>
        =
        failwith ""
        // printfn $"COMBINE  -  a.appGen.value = {a.appGen.value}  -  b.appGen.value = {b.appGen.value}"
        // let (YieldedOrCombined avalues) = a.appGen.value
        // let (Delayed bvalues) = b.appGen.value
        // let (BoxedState astate) = a.appGen.state
        // let (BoxedState bstate) = b.appGen.state
        // { value = YieldedOrCombined (List.append avalues bvalues)
        //   state = List.append (downcast astate) (downcast bstate) }
        // |> toRTAppGen
    member inline _.For(
        s: seq<'ret>,
        body: 'ret -> RTAppGen<YieldedOrCombined<'elem>>)
        : RTAppGen<YieldedOrCombined<'elem>>
        =
        printfn $"FOR"
        failwith "TODO"
        // [ for x in sequence do
        //     yield! body x
        // ]
    member inline _.Zero()
        : RTAppGen<YieldedOrCombined<'elem>>
        =
        // printfn $"ZERO"
        failwith ""
        // { value = YieldedOrCombined []
        //   state = NoState }
        // |> toRTAppGen
    member _.Run(
        children: RTAppGen<Delayed<'elem>>)
        : 'ret
        =
        failwith ""
        // printfn $"RUN"
        // let (Delayed elems) = children.appGen.value
        // { stateType = children.stateType
        //   appGen = { value = elems; state = children.appGen.state} }

let test = Builder()



let a() =
    test {
        let! a = { value = 100; state = "a" }
        let! b = { value = 200; state = 44.2 }
        { value = a + b; state = 20UL }
        
        let! c = { value = 33; state = "c" }
        let! d = { value = 66; state = 44.1 }
        { value = c + d; state = 10.0 }

        { value = -77; state = 20.0 }
        for i in 0..3 do
            { value = -77; state = 20.0 }

        let! e = { value = -2; state = [909090] }
        let! f = { value = -3; state = (0.1, 0.2, 0.3) }
        for i in 0..3 do
            { value = e + f + i; state = ("Hello", "World") }
            { value = e + f + i; state = ("Hello", "World") }
            { value = e + f + i; state = ("Hello", "World") }

        { value = e + f; state = ("Hello", "World") }
    }


let b() =
    test {
        let! a = { value = 100; state = "a" }
        let! b = { value = 200; state = 44.2 }
        { value = a + b; state = 20UL }
        
        let! c = { value = 33; state = "c" }
        let! d = { value = 66; state = 44.1 }
        { value = c + d; state = 10.0 }
        { value = -77; state = 20.0 }
    }


let c() =
    test {
        { value = 33; state = 20UL }
        { value = -77; state = 20.0 }
    }

let d() =
    test {
        { value = -77; state = 20.0 }
    }

let e() =
    test {
        if true then
            { value = -77; state = 20.0 }
    }
