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

[<AutoOpen>]
module DomExtensions =
    type NodeList with
        member this.elements = seq { for i in 0 .. this.length-1 do this.Item i }
    type Node with
        member this.clearChildren() = this.textContent <- "" // TODO: really?

type BoxedState = BoxedState of obj
type RTGen<'v,'r> = { stateType: Type list; appGen: Gen<'v,BoxedState,'r> }

type SyncChildOp =
    | Added of Node * idx: int
    | Removed of int
    | Moved of Node * oldIdx: int * newIdx: int

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
    { stateType = [stateType]
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
// let mapRTGen (proj: 'a -> 'b) (g: RTAppGen<'a>) : RTAppGen<'b> =
//     let a = g.appGen |> Gen.map proj
//     { stateType = g.stateType; appGen = a }

// TODO: Could it be that we neet "toRTAppGen" only in bind?
// TODO: Generalize (App, so that this can be used in any context / framework)
type ViewBuilder<'elem,'ret>([<InlineIfLambda>] run: RTAppGen<'elem> list -> 'ret) =
    member inline _.Bind(
        m: AppGen<'v1,'s1>,
        f: 'v1 -> YieldedOrCombined<RTAppGen<'v2>>)
        : YieldedOrCombined<RTAppGen<'v3>>
        =
        failwith "TODO"
    
    // used for yielding html elements
    member _.Yield(
        x: AppGen<'v,'s>)
        : YieldedOrCombined<RTAppGen<'node>>
        =
        failwith "TODO"
    // used for yielding pov components
    member _.Yield(
        x: RTAppGen<'v> list) // that's the output of `run` when `id` is passed into the builder
        : YieldedOrCombined<RTAppGen<'node>>
        =
        failwith "TODO"

    member _.Return(
        x: 'v)
        : RTAppGen<YieldedOrCombined<'v>>
        =
        failwith "TODO"
    member _.Delay(
        f: unit -> YieldedOrCombined<RTAppGen<'v>>)
        : Delayed<RTAppGen<'v>>
        =
        failwith "TODO"
    member _.Combine(
        a: YieldedOrCombined<RTAppGen<'v>>,
        b: Delayed<RTAppGen<'v>>)
        : YieldedOrCombined<RTAppGen<'v>>
        =
        failwith "TODO"
    member inline _.For(
        s: seq<'a>,
        body: 'a -> YieldedOrCombined<RTAppGen<'b>>)
        : YieldedOrCombined<RTAppGen<'c>>
        =
        failwith "TODO"
    member inline _.Zero()
        : YieldedOrCombined<RTAppGen<'v>>
        =
        failwith "TODO"
    member inline _.Run(children) =
        printfn $"RUN"
        let (Delayed children) = children
        run children

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

    let inline elem<'elem when 'elem :> HTMLElement and 'elem: equality> name attributes (children: RTAppGen<'elem> list) =
        gen {
            let! elem = baseElem<'elem> name
            do syncAttributes elem attributes
            // do! syncChildren elem children
            return elem
        }
    
    let span content =
        gen {
            let! elem = baseElem<HTMLSpanElement> "span"
            do if elem.textContent <> content then
                elem.textContent <- content
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
                return button
            }



let spanInst = span "test"
// TODO: Value restriction
let divInst() : AppGen<_,_> = div [] { () }
let divInst2() = div [] { span "xxxx" }
let buttonInst = button [] id { () }

let test1 =
    pov {
        span "test"
    }

let test2 =
    pov {
        span "test 1"
        span "test 2"
    }

let test3 =
    pov {
        span "test 1"
        div [] {
            ()
        }
        span "test 2"
    }

let test4 =
    pov {
        span "test 1"
        div [] {
            span "inner 1"
            span "inner 2"
        }
        span "test 2"
    }

let test5 =
    pov {
        let! c1, setCount = Gen.ofMutable 0
        span $"c1 = {c1}"

        div [] {
            span "inner 1"
            span "inner 2"
        }
        span "test 2"
        div [] {()}
    }

let test6 =
    pov {
        let! c1,_ = Gen.ofMutable 0
        span $"c1 = {c1}"
        
        let! c2,_ = Gen.ofMutable 0
        div [] {
            span $"c2 = {c2}"
            
            let! c3,_ = Gen.ofMutable 0
            span $"c3 = {c3}"
        }
    }

let test7 =
    pov {
        // TODO: document that this is not working (yield) and not useful. Maybe make a Gen.iter ?
        let! spanElememt = span "test 1"
        printfn $"Span inner text: {spanElememt.innerText}"

        // yield spanElememt
        span "test 2"
    }

let comp =
    pov {
        let! count, setCount = Gen.ofMutable 0
        div [] {
            div []  {
                span $"BEGIN for ..."
                for x in 0..3 do
                    span $"count = {count}"
                    button [] (fun () -> setCount (count + 1)) { 
                        span "..." 
                    }
                    span $"    (another x = {x})"
                    span $"    (another x = {x})"
                span $"END for ..."
            }
        }
    }


let view() =
   pov {
       div [] {
           comp
           div [] {
               span "Hurz"
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

