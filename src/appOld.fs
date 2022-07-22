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
        member this.toSeq = seq { for i in 0 .. this.length-1 do this.Item i }
    type NodeList with
        member this.toList = this.toSeq |> Seq.toList
    type Node with
        member this.clearChildren() = this.textContent <- "" // TODO: really?

type App(document: Document, appElement: Element, triggerUpdate: App -> Node list) as this =
    do
        for elem in triggerUpdate this do
            appElement.appendChild elem |> ignore
    member _.Document = document
    member this.TriggerUpdate() =
        printfn $">>>> Trigger update"
        let element = triggerUpdate this
        // TODO: Sync returned element(s) with current
        ()
    static member Run(view) =
        App(document, document.querySelector("#app"), view |> Gen.toEvaluable)



type AppGen<'v,'s> = Gen<'v,'s,App>
type RTState = State<obj>
type RTAppGen<'v> = AppGen<'v,RTState>

let inline toBoxedGen
    (Gen x: Gen<'v,State<'s>,'r>)
    : Gen<'v,RTState,'r>
    =
    fun s r ->
        let s =
            match s with
            | None -> None
            | Some (s: RTState) -> Some (State.create s.typeChain (s.value :?> 's))
        let xv,xs = x s r
        xv, (State.create xs.typeChain (xs.value :> obj))
    |> Gen

// TODO: Could it be that we neet "toRTAppGen" only in bind?
// TODO: Generalize (App, so that this can be used in any context / framework)
type ViewBuilder<'ret>([<InlineIfLambda>] run: RTAppGen<Node list> -> 'ret) =

    member inline _.Bind(
        m: AppGen<'v1, State<'s1>>,
        f: 'v1 -> AppGen<'v2, State<'s2>>)
        : RTAppGen<'v2>
        =
        Gen.bind m f |> toBoxedGen
    
    member inline _.Yield(
        x: AppGen<'v,State<'s>>)
        : RTAppGen<Node list>
        =
        toBoxedGen x |> Gen.map (fun xv -> [xv :> Node])

    member inline _.Yield(
        x: AppGen<'v list, State<'s>>)
        : RTAppGen<Node list>
        =
        toBoxedGen x |> Gen.map (List.map (fun x -> x :> Node))
    
    member inline _.Delay(
        f: unit -> RTAppGen<Node list>)
        : RTAppGen<Node list>
        =
        f()

    member inline _.Combine(
        a: RTAppGen<Node list>,
        b: RTAppGen<Node list>)
        : RTAppGen<Node list>
        =
        gen {
            let! aNodes = a
            let! bNodes = b
            return List.append aNodes bNodes
        }
        |> toBoxedGen

    member inline this.For(
        s: seq<'a>,
        body: 'a -> RTAppGen<Node list>)
        : RTAppGen<Node list>
        =
        s
        |> Seq.map body
        |> Seq.fold (fun curr next -> this.Combine(curr, next)) (this.Zero())

    member inline _.Zero()
        : RTAppGen<Node list>
        =
        // 's: same reason as in Combine
        Gen.ofValue [] |> toBoxedGen

    member inline _.Run(children) : 'ret =
        run children

let pov = ViewBuilder<_>(id)

[<AutoOpen>]
module HtmlElementsApi =
    let app = Gen.read<App>

    let inline syncAttributes (elem: Node) attributes =
        do for aname,avalue in attributes do
            let elemAttr = elem.attributes.getNamedItem aname
            if elemAttr.value <> avalue then
                elemAttr.value <- avalue

    let inline syncChildren (elem: Node) (children: RTAppGen<Node list>) =
        gen {
            let! children = children
            // TODO: Performance
            do elem.clearChildren()
            do for child in children do
                printfn $"Sync child: {child.nodeName}"
                try elem.appendChild child |> ignore
                with ex ->
                    printfn $"EXCEPTION:::: {ex}"
                    printfn $"    Element is: {elem}"
            return ()
        }

    let inline baseElem<'elem when 'elem :> HTMLElement and 'elem: equality> name =
        gen {
            let! app = app
            let! elem = Gen.preserve (fun () -> app.Document.createElement name :?> 'elem)
            return elem
        }
        // gen {
        //     let! res =
        //         gen {
        //             let! app = app
        //             let! elem = Gen.preserve (fun () -> app.Document.createElement name :?> 'elem)
        //             return elem
        //         }
        //         |> Gen.statesAndValue
        //     let elem = res.value
        //     printfn $"Eval: {elem.nodeName} ({elem.GetHashCode()})"
        //     // printfn $"    IN state:  {res.inState}"
        //     // printfn $"    OUT state: {res.outState}"
        //     return res.value
        // }

    let inline elem<'elem when 'elem :> HTMLElement and 'elem: equality> name attributes (children: RTAppGen<Node list>) =
        gen {
            let! elem = baseElem<'elem> name
            do syncAttributes elem attributes
            do! syncChildren elem children
            return elem
        }
    
    let span content =
        gen {
            let! elem = baseElem<HTMLSpanElement> "span"
            do if elem.textContent <> content then
                elem.textContent <- content
            return elem
        }

    let inline div attributes = ViewBuilder <| elem "div" attributes

    let inline p attributes = ViewBuilder <| elem "p" attributes

    let inline button attributes click =
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

let textInst = span "test"
// TODO: Value restriction
// let divInst = div [] { () }
// let buttonInst = button [] id { () }



let spanInst = span "test"
// TODO: Value restriction
let inline divInst()  = div [] { () }
let inline divInst2() = div [] { span "xxxx" }
let inline buttonInst() = button [] id { () }

module Tests =
    let test1() =
        pov {
            span "test"
        }

    let test2() =
        pov {
            span "test 1"
            span "test 2"
        }

    let test3() =
        pov {
            span "test 1"
            div [] {
                ()
            }
            span "test 2"
        }

    let test4() =
        pov {
            span "test 1"
            div [] {
                span "inner 1"
                span "inner 2"
            }
            span "test 2"
        }

    let test5() =
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

    let test6() =
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

    let test7() =
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

module ViewTest1 =

    // TODO: Example: Parametrized components
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


    let view =
        pov {
            div [] {
                comp
                div [] {
                    span "Hurz"
                    comp
                }
            }
        }
        

module ChangeTypeDuringRuntime =
    let view =
        pov {
            div [] {
                let! count,setCount = Gen.ofMutable 0
                
                p [] { span $"count = {count}     /     count / 5 = {count % 5}" }
                button [] (fun () -> setCount (count + 1)) { 
                    span "Increment (+)" 
                }

                if count % 5 = 0 then
                    div [] {
                        span "YES!!!!"
                        span " ... it's a multiple of 5!"
                    }
                else
                    let! start = Gen.preserve (fun () -> DateTime.Now.ToString())
                    // let! btnCount,setBtnCount = Gen.ofMutable 0
                    div [] {
                        button [] id (*(fun () -> setBtnCount (btnCount + 1))*) {
                            span "You clicked me {btnCount} times" 
                        }
                        span $"State started: {start}"
                    }
            }
        }

App.Run ChangeTypeDuringRuntime.view |> ignore
