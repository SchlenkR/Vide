
[<AutoOpen>]
module Vide.Fable

open System.Runtime.CompilerServices
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

let vide = VideBaseBuilder()

module Mutable =
    type MutableValue<'a>(init: 'a) =
        let mutable state = init
        member val EvaluateView = (fun () -> ()) with get,set
        member this.Value
            with get() = state
            and set(value) = state <- value; this.EvaluateView()

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


type AttributeSyncAction<'a> =
    | Set of 'a
    | Remove
type EventHandler = Event -> unit
type AttributeList = list<string * AttributeSyncAction<string>>
type EventList = list<string * EventHandler>
type NodeBuilderState<'s> = option<Node> * option<'s>

// TODO: Hack
type EventManager() =
    let eventListeners = Fable.Core.JS.Constructors.WeakMap.Create<Node, list<string * EventHandler>>()
    member _.AddListener(node: Node, evtName, handler) =
        node.addEventListener(evtName, handler)
        let registrations =
            if eventListeners.has(node)
                then (evtName, handler) :: eventListeners.get(node)
                else [ evtName, handler ]
        eventListeners.set(node, registrations) |> ignore
    member _.RemoveListener(node: Node, evtName) =
        let registrations =
            if eventListeners.has(node)
                then eventListeners.get(node)
                else []
        for regEvtName,handler in registrations do
            if regEvtName = evtName then
                node.removeEventListener(evtName, handler) |> ignore
        eventListeners
            .set(
                node,
                registrations |> List.filter (fun (n,h) -> n <> evtName))
            |> ignore
let events = EventManager()

type NodeBuilder(getNode: Context -> Node, updateNode: Node -> unit) =
    inherit VideBaseBuilder()
    
    member val Attributes: AttributeList = [] with get, set
    member val Events: EventList = [] with get, set

    member this.Run(
        Vide childVide: Vide<unit,'fs,Context>)
        : Vide<unit, NodeBuilderState<'fs>, Context>
        =
        Vide <| fun s (ctx: Context) ->
            let s,cs = separateStatePair s
            let node =
                match s with
                | None ->
                    getNode ctx
                | Some node ->
                    do
                        ctx.elementsContext.KeepNode(node)
                        updateNode node
                    node
            for name,value in this.Attributes do
                match value with
                | Set value ->
                    let attr = document.createAttribute(name)
                    attr.value <- value
                    node.attributes.setNamedItem(attr) |> ignore
                | Remove ->
                    node.attributes.removeNamedItem(name) |> ignore
            for name,handler in this.Events do
                 events.RemoveListener(node, name)
                 events.AddListener(node, name, handler)
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
            (), Some (Some node, cs)

type HTMLElementBuilder(createNode, updateNode) = inherit NodeBuilder(createNode, updateNode)
type HTMLAnchorElementBuilder(createNode, updateNode) = inherit HTMLElementBuilder(createNode, updateNode)
type HTMLButtonElementBuilder(createNode, updateNode) = inherit HTMLElementBuilder(createNode, updateNode)

[<Extension>]
type NodeBuilderExtensions() =
    [<Extension>]
    static member inline attrCond(this: #NodeBuilder, name, ?value: string) =
        let value =
            match value with
            | Some value -> Set value
            | None -> Remove
        do this.Attributes <- (name, value) :: this.Attributes
        this
    [<Extension>]
    static member inline attrBool(this: #NodeBuilder, name, value: bool) =
        let value =
            match value with
            | true -> Set ""
            | false -> Remove
        do this.Attributes <- (name, value) :: this.Attributes
        this
    [<Extension>]
    static member on(this: #NodeBuilder, name, handler: EventHandler) =
        do this.Events <- (name, handler) :: this.Events
        this

[<Extension>]
type HTMLElementBuilderExtensions() =
    [<Extension>]
    static member inline id(this: #HTMLElementBuilder, ?value) =
        NodeBuilderExtensions.attrCond(this, "id", ?value = value)
    [<Extension>]
    static member inline class'(this: #HTMLElementBuilder, ?value) =
        NodeBuilderExtensions.attrCond(this, "class", ?value = value)
    [<Extension>]
    static member inline hidden(this: #HTMLElementBuilder, value) =
        NodeBuilderExtensions.attrBool(this, "hidden", value)
    
    [<Extension>]
    static member inline onclick(this: #HTMLElementBuilder, handler) =
        NodeBuilderExtensions.on(this, "click", handler)

[<Extension>]
type HTMLAnchorElementBuilderExtensions() =
    [<Extension>]
    static member inline href(this: #HTMLAnchorElementBuilder, ?value: string) =
        // Fable BUG https://github.com/fable-compiler/Fable/issues/3073
        NodeBuilderExtensions.attrCond(this, "href", ?value = value)

let inline element ctor tagName updateNode =
    ctor(
        (fun ctx -> ctx.elementsContext.AddElement(tagName) :> Node),
        updateNode)

// open type (why? -> We need always a new builder)
type Html =
    static member text<'s> text =
        let create (ctx: Context) =
            ctx.elementsContext.AddTextNode(text) :> Node
        let update (node: Node) =
            if node.textContent <> text then node.textContent <- text
        NodeBuilder(create, update)
    static member span = element HTMLElementBuilder "span" ignore
    static member div = element HTMLElementBuilder "div" ignore
    static member p = element HTMLElementBuilder "p" ignore
    static member button = element HTMLButtonElementBuilder "button" ignore
    static member a = element HTMLAnchorElementBuilder "a" ignore

    // TODO: Yield should work for strings

type VideBaseBuilder with
    member inline _.Yield(
        v: NodeBuilder)
        : Vide<unit, NodeBuilderState<unit>, Context>
        =
        v {()}
    member inline _.Yield(
        x: string)
        : Vide<unit, NodeBuilderState<unit> ,Context>
        =
        Html.text x {()}

let start (holder: Node) (v: Vide<unit,'s,Context>) =
    let ctx =
        {
            node = holder
            evaluateView = fun () -> ()
            elementsContext = ElementsContext(holder)
        }
    let evaluate =
        NodeBuilder((fun _ -> holder), ignore) { v }
        |> toStateMachine None ctx
    do ctx.evaluateView <- evaluate
    evaluate()
