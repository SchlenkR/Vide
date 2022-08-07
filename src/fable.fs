
[<AutoOpen>]
module Fiu.Fable

open System.Linq
open Fiu
open Browser
open Browser.Types

[<AutoOpen>]
module DomExtensions =
    type NodeList with 
        member this.ToSeq() = seq { for i in 0 .. this.length-1 do this.Item i }
        member this.ToList() = this.ToSeq() |> Seq.toList

type Context(parent: Node) =
    let mutable keptNodes = []
    let memory x =
        keptNodes <- (x :> Node) :: keptNodes
        x
    member _.addElement(tagName: string) =
        document.createElement tagName |> memory
    member _.addTextNode(text: string) =
        document.createTextNode text |> memory
    member _.keepNode(element: Node) =
        element |> memory |> ignore
    member _.GetObsoleteNodes() =
        parent.childNodes.ToList() |> List.except keptNodes

let fiu<'s> = FiuBuilder<'s,'s,Context>(id)

let inline node
    (create: Context -> Node)
    (update: Node -> unit)
    (attributes: list<string * string>)
    (events: list<string * (Event -> unit)>)
    : FiuBuilder<'s, option<Node * list<string * string>> * option<'s>, Context>
    =
    let run (Fiu childFiu) =
        Fiu <| fun s (ctx: Context) ->
            console.log("EXEC")
            let s,cs = unwrapTupledState s
            let node,oldAttributes =
                match s with
                | None ->
                    let node = create ctx
                    // The event list is considered invariant and is thus only evaluated initially
                    for name,handler in events do
                        node.addEventListener(name, handler)
                    node,[]
                | Some (node,oldAttributes) ->
                    do ctx.keepNode node
                    do update node
                    node,oldAttributes
            do
                // TODO: Performance all over the place
                let removedAttrs =
                    oldAttributes 
                    |> List.filter (fun a -> 
                        attributes 
                        |> List.exists (fun a' -> fst a' = fst a) 
                        |> not)
                for attr,_ in removedAttrs do
                    do node.attributes.removeNamedItem(attr) |> ignore
                for attrName,attrValue in attributes do
                    let attr = document.createAttribute(attrName)
                    attr.value <- attrValue
                    do node.attributes.setNamedItem(attr) |> ignore
            let ctx = Context(node)
            let cv,cs = childFiu cs ctx
            for x in ctx.GetObsoleteNodes() do
                node.removeChild(x) |> ignore
            (), Some (Some (node,attributes), cs)
    FiuBuilder(run)
    
let inline element tagName attributes events =
    node (fun ctx -> ctx.addElement tagName) ignore attributes events

module Html =
    let inline text text attributes events =
        let create (ctx: Context) =
            ctx.addTextNode text :> Node
        let update (node: Node) =
            if node.textContent <> text then
                node.textContent <- text
        node create update attributes events
    let inline div attributes events = element "div" attributes events
    let inline p attributes events = element "p" attributes events

    // TODO: Yield should work for strings

type FinalState<'s> = option<'s> * option<unit>

type FiuBuilder<'fs1,'fs2,'c> with
    member inline _.Yield(
        f: FiuBuilder<'s1, FinalState<'s2>, 'c>)
        : Fiu<unit, FinalState<'s2>, 'c>
        =
        printMethod "Yield (FiuBuilder)"
        let res = f { () }
        res
    member inline _.Yield(
        x: string)
        : Fiu<unit, FinalState<Node * list<string * string>>, Context>
        =
        printMethod "Yield (string)"
        Html.text x [] [] { () }

let start (holder: HTMLElement) (fiu: Fiu<unit,'s,Context>) =
    let evaluate = fiu |> toStateMachine None (Context holder)
    evaluate()
