// TODOs:
//  - Don't calc the whole tree when triggering Update
//  - first class task/async support (in gen)
//  - implement "for" in ChildBuilder
//  - hide all the crazy generic type signatures
// - use #Node instead of Node

module App

open System

open LocSta
open LocSta.Gen

open Browser
open Browser.Types

[<AutoOpen>]
module Application =
    type App(document: Document, appElement: Element, triggerUpdate: App -> Node) =
        member _.Document = document
        member this.Run() =
            let initialElement = triggerUpdate this
            appElement.appendChild initialElement |> ignore
        member this.TriggerUpdate() =
            printfn $"Trigger update"
            let element = triggerUpdate this
            // TODO: Sync returned element(s) with current
            ()
    let app = Gen (fun s (r: App) -> r,())

[<AutoOpen>]
module HelperAndExtensions =
    type NodeList with
        member this.elements = seq { for i in 0 .. this.length-1 do this.Item i }
    type Node with
        member this.clearChildren() = this.textContent <- "" // TODO: really?

[<AutoOpen>]
module Framework =

    type RuntimeTypedAppGen<'o> = Type * Gen<'o,obj,App>

    let inline syncAttributes (elem: Node) attributes =
        do for aname,avalue in attributes do
            let elemAttr = elem.attributes.getNamedItem aname
            if elemAttr.value <> avalue then
                elemAttr.value <- avalue

    let inline syncChildren (elem: Node) (children: RuntimeTypedAppGen<_> list) =
        Gen <| fun s r ->
            let s = s |> Option.defaultWith (fun () -> ResizeArray())
            let newState =
                [ for childType, (Gen childGen) in children do
                    let stateIdx = s |> Seq.tryFindIndex (fun (typ,_) -> typ = childType)
                    let newChildState =
                        match stateIdx with
                        | Some idx ->
                            let childState = s[idx]
                            do s.RemoveAt(idx)
                            childGen (childState |> snd |> Some) r |> snd
                        | None ->
                            let o,s = childGen None r
                            do elem.appendChild o |> ignore
                            s
                    yield childType,newChildState
                ]
            (), ResizeArray newState

    let inline boxGen (stateType: Type) (Gen g: Gen<'o,'s,App>) : RuntimeTypedAppGen<'o> =
        // fable requires runtime-resolution and passing the stateType from callsite due to erasure
        let g = Gen <| fun s r ->
            let o,s = g (unbox s) r
            o, box s
        stateType, g

    // TODO: Add overloads for yield (string, int, etc.)
    type ChildrenBuilder<'o,'s when 'o :> Node>(nodeGen: Gen<'o,'s,App>) =
        member inline _.Yield<'o,'s1>(x: Gen<'o,'s1,App>) = [boxGen typeof<'s1> x]
        member inline _.YieldFrom<'o,'s1>(x: Gen<'o list,'s1,App>) = [boxGen typeof<'s1> x]
        member inline _.Delay([<InlineIfLambda>] f) = f ()
        member _.Combine(a, b) = List.append a b
        member _.Zero() = []
        member _.Run<'o when 'o :> Node>(children: list<RuntimeTypedAppGen<'o>>) =
            loop {
                let! node = nodeGen
                do! syncChildren node children
                return node
            }

        member inline _.For(sequence: seq<'a>, body: 'a -> RuntimeTypedAppGen<'o> list) : RuntimeTypedAppGen<'o> list =
            [ for x in sequence do yield! body x ]

    type ViewBuilder() =
        member inline _.Bind(m, [<InlineIfLambda>] f) = Gen.bind m f
        member _.Yield(x: Gen<#Node,'s,App>) = x
        member inline _.Delay([<InlineIfLambda>] f) = f ()
        member _.Combine(a, b: Bottom) : Gen<_,_,_> = a
        member _.Zero() = Bottom.Instance
    and Bottom private () = static member internal Instance = Bottom()
    let pview = ViewBuilder()


[<AutoOpen>]
module HtmlElementsApi =

    let inline elem name attributes =
        loop {
            let! app = app
            let! elem = preserve (fun () -> app.Document.createElement name :> Node)
            printfn $"Eval: {name} ({elem.GetHashCode()})"
            do syncAttributes elem attributes
            return elem
        }

    let text text =
        loop {
            let! app = app
            let! elem = preserve (fun () -> app.Document.createTextNode text)
            do if elem.textContent <> text then
                elem.textContent <- text
            return elem :> Node
        }

    let div attributes = ChildrenBuilder(elem "div" attributes)

    let p attributes = ChildrenBuilder(elem "p" attributes)

    let button attributes click =
        ChildrenBuilder <| loop {
            let! app = app
            let! button =
                elem "button" attributes
                |> Gen.map (fun x -> x :?> HTMLButtonElement) // TODO: Cast + map necessary?
            button.onclick <- fun _ ->
                printfn "-----CLICK"
                click ()
                app.TriggerUpdate()
            return button :> Node // TODO: It's crap that we have to cast everything to "Node"
        }

    let empty = text ""


let comp =
    pview {
        let! count, setCount = Gen.ofMutable 0
        div [] {
            div []  {
                text $"BEGIN for ..."
                for x in 0..3 do
                    text $"count = {count}"
                    button [] (fun () -> setCount (count + 1)) { text "..." }
                    text $"    (another x = {x})"
                    text $"    (another x = {x})"
                text $"END for ..."
            }
        }
    }


let view() =
   pview {
       div [] {
           comp
           div [] {
               text "Hurz"
               comp
           }
       }
   }
    

do
   App(
       document,
       document.querySelector("#app"),
       view() |> Gen.toEvaluable
   ).Run()
