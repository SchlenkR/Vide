// TODOs:
//  - Don't calc the whole tree when triggering Update
//  - first class task/async support (in gen)
//  - Generalize (App, so that this can be used in any context / framework)

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

type BoxedState = BoxedState of obj
type CompoundState<'s> = { mState: 's; fStates: BoxedState list }
type RTGen<'v,'s,'r> = { stateType: Type; appGen: Gen<'v,'s,'r> }

// TODO: Do we really need both? What they discriminate?
type YieldedOrCombined<'a> = YieldedOrCombined of 'a list
type Delayed<'a> = Delayed of 'a list


type ViewBuilder<'elem,'ret>([<InlineIfLambda>] run: RTGen<'elem,BoxedState,App> list -> 'ret) =
    member inline _.Bind(
        Gen m: Gen<'v1,'s,App>,
        f: 'v1 -> YieldedOrCombined<RTGen<'v2,BoxedState,App>>)
        : YieldedOrCombined<RTGen<'v2,BoxedState,App>>
        =
        failwith "TODO"

    // used for yielding html elements
    member _.Yield(
        x: Gen<'v,'s,App>)
        : YieldedOrCombined<RTGen<'node,BoxedState,App>>
        =
        failwith "TODO"

    // used for yielding pov components
    member _.Yield(
        x: RTGen<'v,BoxedState,App> list) // that's the output of `run` when `id` is passed into the builder
        : YieldedOrCombined<RTGen<'node,BoxedState,App>>
        =
        failwith "TODO"

    member _.Delay(
        f: unit -> YieldedOrCombined<RTGen<'v,BoxedState,App>>)
        : Delayed<RTGen<'v,BoxedState,App>>
        =
        failwith "TODO"
    member _.Combine(
        a: YieldedOrCombined<RTGen<'v,BoxedState,App>>,
        b: Delayed<RTGen<'v,BoxedState,App>>)
        : YieldedOrCombined<RTGen<'v,BoxedState,App>>
        =
        failwith "TODO"
    member inline _.For(
        s: seq<'a>,
        body: 'a -> YieldedOrCombined<RTGen<'b,BoxedState,App>>)
        : YieldedOrCombined<RTGen<'c,BoxedState,App>>
        =
        failwith "TODO"
    member inline _.Zero()
        : YieldedOrCombined<RTGen<'v,BoxedState,App>>
        =
        failwith "TODO"
    member inline _.Run(children) =
        printfn $"RUN"
        let (Delayed children) = children
        run children

let pov = ViewBuilder<Node,_>(id)

let app : Gen<_,_,App> = Gen (fun s r -> r,NoState)



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

    let inline elem<'elem when 'elem :> HTMLElement and 'elem: equality> name attributes (children: RTGen<'elem,BoxedState,App> list) =
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
let divInst() : Gen<_,_,App> = div [] { () }
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
        // TODO: document that this is not working (yield) and not useful.
        // - Maybe Gen.iter?
        // - or `wrap` to emit the spanElement afterwards?
        // - make also a "preserve" example
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

