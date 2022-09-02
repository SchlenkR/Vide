
[<AutoOpen>]
module Vide.Fable

open Fable.Core.JS
open Browser
open Browser.Types
open Vide

type NodeList with 
    member this.ToSeq() = seq { for i in 0 .. this.length-1 do this.Item i }
    member this.ToList() = this.ToSeq() |> Seq.toList

module NodeExt =
    let displayString (node: Node) =
        let idOrDefault = try node.attributes.getNamedItem("id").value with _ -> "--"
        $"<{node.nodeName} id='{idOrDefault}'>"

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
    member _.KeepNode(node: Node) =
        node |> memory |> ignore
    member _.GetObsoleteNodes() =
        let childNodes = parent.childNodes.ToList()
        childNodes |> List.except keptNodes

module Mutable =
    type MutableValue<'a>(init: 'a) =
        let mutable state = init
        member val EvaluateView = (fun () -> ()) with get,set
        member this.Set(value) = state <- value; this.EvaluateView()
        member this.Value
            with get() = state
            and set(value) = this.Set(value)

    let inline change op (mutVal: MutableValue<_>) x =
        mutVal.Value <- op mutVal.Value x
    
    let ofValue x =
        Vide <| fun s (c: Context) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x))
            do s.EvaluateView <- c.evaluateView
            s, Some s
    
    //let list x =
    //    Vide <| fun s (c: Context) ->
    //        let s = s |> Option.defaultWith (fun () -> MutableValue(x))
    //        do s.EvaluateView <- c.evaluateView
    //        s, Some s

let inline ( += ) mutVal x = Mutable.change (+) mutVal x
let inline ( -= ) mutVal x = Mutable.change (-) mutVal x
let inline ( *= ) mutVal x = Mutable.change (*) mutVal x
let inline ( /= ) mutVal x = Mutable.change (/) mutVal x
let inline ( := ) (mutVal: Mutable.MutableValue<_>) x = mutVal.Value <- x

type AttributeSyncAction<'a> =
    | Set of 'a
    | Remove
type EventHandler = Event -> unit
type AttributeList = list<string * AttributeSyncAction<string>>
type EventList = list<string * EventHandler>
type NodeBuilderState<'s, 'n when 'n :> Node> = option<'n> * option<'s>
type NodeCheckResult = Keep | DiscardAndCreateNew

// TODO: Hack? Is there a better way?
type EventManager() =
    let listeners = Constructors.WeakMap.Create<Node, list<string * EventHandler>>()
    member _.AddListener(node: Node, evtName, handler) =
        node.addEventListener(evtName, handler)
        let registrations =
            if listeners.has(node)
                then (evtName, handler) :: listeners.get(node)
                else [ evtName, handler ]
        listeners.set(node, registrations) |> ignore
    member _.RemoveListener(node: Node, evtName) =
        let registrations = if listeners.has(node) then listeners.get(node) else []
        for regEvtName,handler in registrations do
            if regEvtName = evtName then
                node.removeEventListener(evtName, handler) |> ignore
        listeners
            .set(
                node,
                registrations |> List.filter (fun (n,h) -> n <> evtName))
            |> ignore

let eventManager = EventManager()

type NodeBaseBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
    inherit VideBuilder()
    member val Attributes: AttributeList = [] with get, set
    member val Events: EventList = [] with get, set
    member this.SyncNode
        (Vide childVide: Vide<unit,'fs,Context>)
        : Vide<'n, NodeBuilderState<'fs, 'n>, Context>
        =
        let syncAttrs (node: Node) =
            for name,value in this.Attributes do
                match value with
                | Set value ->
                    let attr = document.createAttribute(name)
                    attr.value <- value
                    node.attributes.setNamedItem(attr) |> ignore
                | Remove ->
                    node.attributes.removeNamedItem(name) |> ignore
        let syncEvents (node: Node) =
            for name,handler in this.Events do
                 eventManager.RemoveListener(node, name)
                 eventManager.AddListener(node, name, handler)
        Vide <| fun s (ctx: Context) ->
            let s,cs = separateStatePair s
            let node,cs =
                match s with
                | None ->
                    newNode ctx,cs
                | Some node ->
                    match checkOrUpdateNode node with
                    | Keep ->
                        ctx.elementsContext.KeepNode(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        newNode ctx,None
            do syncAttrs node
            do syncEvents node
            let childCtx =
                {
                    node = node
                    evaluateView = ctx.evaluateView
                    elementsContext = ElementsContext(node)
                }
            let cv,cs = childVide cs childCtx
            for x in childCtx.elementsContext.GetObsoleteNodes() do
                node.removeChild(x) |> ignore
                // we don'tneed this? Weak enough?
                // events.RemoveListener(node)
            node, Some (Some node, cs)

type EmitNodeBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
    inherit NodeBaseBuilder<'n>(newNode, checkOrUpdateNode)
    member this.Run
        (
            childVide: Vide<unit,'fs,Context>
        ) : Vide<'n, NodeBuilderState<'fs, 'n>, Context>
        =
        this.SyncNode(childVide)

type DiscardNodeBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
    inherit NodeBaseBuilder<'n>(newNode, checkOrUpdateNode)
    member this.Run
        (
            childVide: Vide<unit,'fs,Context>
        ) : Vide<unit, NodeBuilderState<'fs, 'n>, Context>
        =
        this.SyncNode(childVide) |> map ignore

let inline prepareStart (holder: #Node) (v: Vide<unit,'s,Context>) onEvaluated =
    let ctx =
        {
            node = holder
            evaluateView = fun () -> ()
            elementsContext = ElementsContext(holder)
        }
    let videMachine =
        VideMachine(
            None,
            ctx,
            DiscardNodeBuilder((fun _ -> holder), fun _ -> Keep) { v },
            onEvaluated)
    do ctx.evaluateView <- videMachine.Eval
    videMachine

let vide = VideBuilder()
