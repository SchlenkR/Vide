
[<AutoOpen>]
module Vide.Fable

open Vide
open Browser
open Browser.Types

let inline internal log (o: obj) =
    console.log(o)
    // ()

[<AutoOpen>]
module DomExtensions =
    type NodeList with 
        member this.ToSeq() = seq { for i in 0 .. this.length-1 do this.Item i }
        member this.ToList() = this.ToSeq() |> Seq.toList

type Context =
    {
        node: Node
        mutable evaluateView: unit -> unit
        mutable elementsContext: ElementsContext
    }

and ElementsContext(parent: Node) =
    let mutable keptNodes = []
    let memory x =
        keptNodes <- (x :> Node) :: keptNodes
        x
    let append x =
        do parent.appendChild(x) |> ignore
        x
    member _.AddElement(tagName: string) =
        document.createElement tagName |> memory |> append
    member _.AddTextNode(text: string) =
        document.createTextNode text |> memory |> append
    member _.KeepNode(element: Node) =
        element |> memory |> ignore
    member _.GetObsoleteNodes() =
        parent.childNodes.ToList() |> List.except keptNodes

type VideBuilder<'fs1>() =
    inherit VideBaseBuilder<'fs1>()

let vide<'fs> = VideBuilder<'fs>()

type MutableState<'a>(init: 'a) =
    let mutable x = init
    member val EvaluateView = (fun () -> ()) with get,set
    member this.Value
        with get() = x
        and set(value) = x <- value; this.EvaluateView()

let state x =
    Vide <| fun s (c: Context) ->
        let s = s |> Option.defaultWith (fun () -> MutableState(x))
        do s.EvaluateView <- c.evaluateView
        s, Some s

type EventHandler = Event -> Unit
type AttributeList = list<string * string>
type EventList = list<string * EventHandler>
type NodeBuilderState<'s> = option<Node * AttributeList * EventList> * option<'s>

type NodeBuilder<'fs>(
    createNode: Context -> Node,
    updateNode: Node -> unit,
    attributes: list<string * string>,
    events: list<string * (Event -> unit)>)
    =
    inherit VideBaseBuilder<'fs>()
    //inherit VideBaseBuilder<'fs, NodeBuilderState<'fs>, Context>(this.DoRun)
    member _.Run(
        Vide childVide: Vide<unit,'fs,Context>)
        : Vide<unit, NodeBuilderState<'fs>, Context>
        =
        log "Run"
        Vide <| fun s (ctx: Context) ->
            let s,cs = separateStatePair s
            let node,oldAttributes,oldEvents =
                match s with
                | None ->
                    let node = createNode ctx
                    for name,handler in events do
                        node.addEventListener(name, handler) |> ignore
                    node,[],[]
                | Some (node,oldAttributes,oldEvents) ->
                    do
                        ctx.elementsContext.KeepNode(node)
                        updateNode node
                    node,oldAttributes,oldEvents
            let evaluate () =
                // TODO: Performance all over the place
                let except currents olds =
                    [ for a in olds do
                        if currents |> List.exists (fun a' -> fst a' = fst a) |> not
                            then yield a ]
                do
                    let removedAttrs = oldAttributes |> except attributes
                    for name,_ in removedAttrs do
                        node.attributes.removeNamedItem(name) |> ignore
                    for name,value in attributes do
                        let attr = document.createAttribute(name)
                        attr.value <- value
                        node.attributes.setNamedItem(attr) |> ignore
                // TODO: Think about variant event declarations
                // do
                //     let removedEvents = oldEvents |> except events
                //     for name,handler in removedEvents do
                //         node.removeEventListener(name, handler) |> ignore
                //     for name,handler in events do
                //         node.addEventListener(name, handler) |> ignore
                let childCtx =
                    {
                        node = node
                        evaluateView = ctx.evaluateView
                        elementsContext = ElementsContext(node)
                    }
                let cv,cs = childVide cs childCtx
                for x in childCtx.elementsContext.GetObsoleteNodes() do
                    node.removeChild(x) |> ignore
                cv,cs
            let cv,cs = evaluate()
            (), Some (Some (node,attributes,events), cs)

let inline node
    (createNode: Context -> Node)
    (updateNode: Node -> unit)
    (attributes: list<string * string>)
    (events: list<string * (Event -> unit)>)
    : NodeBuilder<'s>
    =
    NodeBuilder(createNode, updateNode, attributes, events)
    
let inline element tagName attributes events =
    node (fun ctx -> ctx.elementsContext.AddElement tagName) ignore attributes events

module Html =
    let text text attributes events =
        let create (ctx: Context) =
            ctx.elementsContext.AddTextNode text :> Node
        let update (node: Node) =
            if node.textContent <> text then
                node.textContent <- text
        node create update attributes events
    let div attributes events = element "div" attributes events
    let p attributes events = element "p" attributes events
    let button attributes events = element "button" attributes events

    // TODO: Yield should work for strings

type VideBaseBuilder<'fs1> with
    member inline _.Yield(
        v: NodeBuilder<unit>)
        : Vide<unit, NodeBuilderState<unit>, Context>
        =
        log "Yield (VideBuilder)"
        let res = v { () }
        res
    member inline _.Yield(
        x: string)
        : Vide<unit, NodeBuilderState<unit> ,Context>
        =
        log "Yield (string)"
        Html.text x [] [] { () }

let start (holder: HTMLElement) (vide: Vide<unit,'s,Context>) =
    let ctx =
        {
            node = holder
            evaluateView = fun () -> ()
            elementsContext = ElementsContext(holder)
        }
    let evaluate = vide |> toStateMachine None ctx
    do ctx.evaluateView <- evaluate
    evaluate()
