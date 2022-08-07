
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

type FiuBuilder<'fs1,'fs2,'c> with
    member inline _.Yield(
        x: FiuBuilder<'s, Node option * unit option, 'c>)
        : Gen<unit, Node option * unit option, 'c>
        =
        printMethod "Yield (FiuBuilder)"
        x { () }

let fiu<'s> = FiuBuilder<'s,'s,Context>(id)

let inline node
    (create: Context -> Node) 
    (update: Node -> unit) 
    (attributes: list<string * string>)
    (events: list<unit -> unit>)
    =
    let run (Gen childGen) =
        Gen <| fun s (ctx: Context) ->
            let s,cs = unwrapTupledState s
            let node,oldAttributes =
                match s with
                | None ->
                    let node = create ctx
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
            let cv,cs = childGen cs ctx
            for x in ctx.GetObsoleteNodes() do
                node.removeChild(x) |> ignore
            (), Some (Some (node,attributes), cs)
    FiuBuilder(run)
    
let inline element tagName attributes =
    node (fun ctx -> ctx.addElement tagName) ignore attributes

module Html =
    let inline text text attributes =
        let create (ctx: Context) =
            ctx.addTextNode text :> Node
        let update (node: Node) =
            if node.textContent <> text then
                node.textContent <- text
        node create update attributes
    let inline div attributes = element "div" attributes
    let inline p attributes = element "p" attributes

    // TODO: Yield should work for strings

type FiuBuilder<'fs1,'fs2,'c> with
    member inline _.Yield(
        x: string)
        : Gen<_,_,_>
        =
        printMethod "Yield (string)"
        Html.text x [] { () }
