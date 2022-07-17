// TODOs:
//  - Don't calc the whole tree when triggering Update
//  - first class task/async support (in gen)
//  - implement "for" in ChildBuilder
//  - hide all the crazy generic type signatures

module App

open System
open LocSta
open Browser
open Browser.Types

type BoxedState = BoxedState of obj
type RTGen<'v,'r> = { stateType: Type; appGen: Gen<'v,BoxedState,'r> }

type SyncChildOp =
    | Added of Node * idx: int
    | Removed of int
    | Moved of Node * oldIdx: int * newIdx: int

[<AutoOpen>]
module DomExtensions =
    type NodeList with
        member this.elements = seq { for i in 0 .. this.length-1 do this.Item i }
    type Node with
        member this.clearChildren() = this.textContent <- "" // TODO: really?

// let inline syncChildren (elem: Node) (children: RTGen<'elem list, 'r>) =
//     Gen <| fun s r ->
//         let s = s |> Option.defaultWith (fun () -> []) |> List.indexed
//         let mutable removedIndexes = []
//         let elementsAndState =
//             [  for newIdx,child in children |> List.indexed do
//                 let state = 
//                     s 
//                     |> List.filter (fun (i,(typ,_)) -> 
//                         removedIndexes |> List.contains i |> not
//                         && typ = childType
//                     )
//                     |> List.tryHead
//                 let syncOp,newChildState =
//                     match state with
//                     | Some (lastIdx, (typ, childState)) ->
//                         do removedIndexes <- lastIdx :: removedIndexes
//                         let child,newChildState = childGen (Some childState) r
//                         Moved (child,lastIdx,newIdx), newChildState
//                     | None ->
//                         let child,newChildState = childGen None r
//                         Added (child,newIdx), newChildState
//                 yield syncOp, Some (childType,newChildState)                 ]
//             @ [ for lastIdx,_ in s do
//                 if removedIndexes |> List.contains lastIdx then
//                     Removed lastIdx, None ]
//         let syncOps = elementsAndState |> List.map fst
//         let newState = elementsAndState |> List.map snd |> List.choose id
//         syncOps, newState

type App(document: Document, appElement: Element, triggerUpdate: App -> Node list) =
    member _.Document = document
    member this.Run() =
        for elem in triggerUpdate this do
            appElement.appendChild elem |> ignore
    member this.TriggerUpdate() =
        printfn $"Trigger update"
        let element = triggerUpdate this
        // TODO: Sync returned element(s) with current
        ()

type AppGen<'v,'s> = Gen<'v,'s,App>
type BoxedAppGen<'v> = AppGen<'v,BoxedState>
type RTAppGen<'v> = RTGen<'v,App>
type YieldedOrCombined<'a> = YieldedOrCombined of 'a list
type Delayed<'a> = Delayed of 'a list


let runDelayed (Delayed x) = x
let runBoxedState (BoxedState x) = x
let toRTAppGen (stateType: Type) (g: AppGen<'v,'s>) : RTAppGen<'v> =
    // fable requires runtime-resolution and passing the stateType from callsite due to erasure
    let g : BoxedAppGen<'v> =
        Gen <| fun s r ->
            let (Gen g) = g
            let o,s = g (unbox s) r
            o, BoxedState s
    { stateType = stateType
      appGen = g }
// let inline unboxAppGen<'v,'s> (g: RTAppGen<'v>) : AppGen<'v,'s> =
//     fun s r ->
//         let (Gen g) = g.appGen
//         match s with
//         | Some s ->
//             let gres = g (BoxedState s) r
//             gres
//     |> Gen

// let ofValue v : AppGen<_,_> = { value = v; state = NoState }
let mapRTGen (proj: 'a -> 'b) (g: RTAppGen<'a>) : RTAppGen<'b> =
    let a = g.appGen |> Gen.map proj
    { stateType = g.stateType; appGen = a }

// TODO: Could it be that we neet "toRTAppGen" only in bind?
// TODO: Generalize (App, so that this can be used in any context / framework)
type ViewBuilder<'elem,'ret>([<InlineIfLambda>] run: RTAppGen<'elem list> -> 'ret) =
    member inline _.Bind(
        m: AppGen<'v1,'s1>,
        f: 'v1 -> RTAppGen<'v2>)
        : RTAppGen<'v3>
        =
        failwith "TODO"
        // printfn $"BIND     -  m.value = {m.value}"
        // let fres = bind m' f
        // { value = fres.value
        //   state = m.state, fres.state }
        // |> toRTAppGen
    member _.Yield(
        x: AppGen<'v,'s>)
        : RTAppGen<YieldedOrCombined<'node>>
        =
        failwith "TODO"
        // printfn $"YIELD    -  x.value = {x.value}"
        // { value = YieldedOrCombined [x.value]
        //   state = x.state }
        // |> toRTAppGen
    member _.Yield(
        x: RTAppGen<'v>)
        : RTAppGen<YieldedOrCombined<'node>>
        =
        failwith "TODO"
        // printfn $"YIELD    -  x.value = {x.value}"
        // { value = YieldedOrCombined [x.value]
        //   state = x.state }
        // |> toRTAppGen
    // member _.Return(
    //     x: 'v)
    //     : RTAppGen<YieldedOrCombined<'v>>
    //     =
    //     failwith "TODO"
    member _.Delay(
        f: unit -> RTAppGen<YieldedOrCombined<'v>>)
        : RTAppGen<Delayed<'v>>
        =
        failwith "TODO"
        // let fres = f()
        // let (YieldedOrCombined fvalue) = fres.appGen.value
        // printfn $"DELAY    -  f() = {fvalue}"
        // { value = Delayed fvalue
        //   state = fres.appGen.state }
        // |> toRTAppGen
    member _.Combine(
        a: RTAppGen<YieldedOrCombined<'v>>,
        b: RTAppGen<Delayed<'v>>)
        : RTAppGen<YieldedOrCombined<'v>>
        =
        failwith "TODO"
        // printfn $"COMBINE  -  a.appGen.value = {a.appGen.value}  -  b.appGen.value = {b.appGen.value}"
        // let (YieldedOrCombined avalues) = a.appGen.value
        // let (Delayed bvalues) = b.appGen.value
        // let (BoxedState astate) = a.appGen.state
        // let (BoxedState bstate) = b.appGen.state
        // { value = YieldedOrCombined (List.append avalues bvalues)
        //   state = List.append (downcast astate) (downcast bstate) }
        // |> toRTAppGen
    member inline _.For(
        s: seq<'a>,
        body: 'a -> RTAppGen<YieldedOrCombined<'b>>)
        : RTAppGen<YieldedOrCombined<'c>>
        =
        failwith "TODO"
        // [ for x in sequence do
        //     yield! body x
        // ]
    member inline _.Zero()
        : RTAppGen<YieldedOrCombined<'v>>
        =
        failwith "TODO"
        // printfn $"ZERO"
        // { value = YieldedOrCombined []
        //   state = NoState }
        // |> toRTAppGen
    member inline _.Run(children) =
        printfn $"RUN"
        children |> mapRTGen runDelayed

let pov = ViewBuilder<Node,_>(id)

let app : AppGen<_,_> = Gen (fun s r -> r,NoState)



[<AutoOpen>]
module HtmlElementsApi =
    let inline syncAttributes (elem: Node) attributes =
        do for aname,avalue in attributes do
            let elemAttr = elem.attributes.getNamedItem aname
            if elemAttr.value <> avalue then
                elemAttr.value <- avalue

    let inline baseElem<'elem when 'elem :> HTMLElement and 'elem: equality> name =
        gen {
            let! app = app
            let! elem = Gen.preserve (fun () -> app.Document.createElement name :?> 'elem)
            printfn $"Eval: {name} ({elem.GetHashCode()})"
            return elem
        }

    let inline elem<'elem when 'elem :> HTMLElement and 'elem: equality> name attributes (children: RTAppGen<'elem list>) =
        gen {
            let! elem = baseElem<'elem> name
            do syncAttributes elem attributes
            // do! syncChildren elem children
            return elem
        }
    
    let text text =
        gen {
            let! elem = baseElem<HTMLSpanElement> "span"
            do if elem.textContent <> text then
                elem.textContent <- text
            return elem
        }

    let div attributes = ViewBuilder <| elem "div" attributes

    let p attributes = ViewBuilder <| elem "p" attributes

    let button attributes click =
        ViewBuilder <| fun children ->
            gen {
                let! app = app
                let! button = elem<HTMLButtonElement> "button" attributes children
                button.onclick <- fun _ ->
                    printfn "-----CLICK"
                    click ()
                    app.TriggerUpdate()
                return button :> Node // TODO: It's crap that we have to cast everything to "Node"
            }

let textInst = text "test"
// TODO: Value restriction
// let divInst = div [] { () }
// let buttonInst = button [] id { () }

let comp : RTAppGen<Node list> =
    pov {
        let! count, setCount = Gen.ofMutable 0
        div [] {
            div []  {
                text $"BEGIN for ..."
                for x in 0..3 do
                    text $"count = {count}"
                    button [] (fun () -> setCount (count + 1)) { 
                        text "..." 
                    }
                    text $"    (another x = {x})"
                    text $"    (another x = {x})"
                text $"END for ..."
            }
        }
    }


let view() =
   pov {
       div [] {
           comp
           div [] {
               text "Hurz"
               comp
           }
       }
   }
    

// do
//    App(
//        document,
//        document.querySelector("#app"),
//        view() |> Gen.toEvaluable
//    ).Run()

