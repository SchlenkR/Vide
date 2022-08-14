
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

type VideBuilder() =
    inherit VideBaseBuilder()

let vide = VideBuilder()

type MutableState<'a>(init: 'a) =
    let mutable state = init
    member val EvaluateView = (fun () -> ()) with get,set
    member this.value
        with get() = state
        and set(value) = state <- value; this.EvaluateView()

let state x =
    Vide <| fun s (c: Context) ->
        let s = s |> Option.defaultWith (fun () -> MutableState(x))
        do s.EvaluateView <- c.evaluateView
        s, Some s

type EventHandler = Event -> Unit
type AttributeList = list<string * string option>
type EventList = list<string * EventHandler>
type NodeBuilderState<'s> = option<Node * AttributeList * EventList> * option<'s>

type NodeBuilder(createNode: Context -> Node, updateNode: Node -> unit) =
    inherit VideBaseBuilder()
    
    member val Attributes: AttributeList = [] with get, set
    member val Events: EventList = [] with get, set

    member this.Run(
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
                    for name,handler in this.Events do
                        log("ADD LISTENER")
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
                    let removedAttrs = oldAttributes |> except this.Attributes
                    for name,_ in removedAttrs do
                        node.attributes.removeNamedItem(name) |> ignore
                    for name,value in this.Attributes do
                        let attr = document.createAttribute(name)
                        match value with
                        | Some value -> attr.value <- value
                        | None -> ()
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
            (), Some (Some (node, this.Attributes, this.Events), cs)

type HTMLElementBuilder(createNode, updateNode) = inherit NodeBuilder(createNode, updateNode)
type HTMLAnchorElementBuilder(createNode, updateNode) = inherit NodeBuilder(createNode, updateNode)
type HTMLButtonElementBuilder(createNode, updateNode) = inherit NodeBuilder(createNode, updateNode)

open System.Runtime.CompilerServices

[<Extension>]
type NodeBuilderExtensions() =
    [<Extension>]
    static member inline attrCond(this: #NodeBuilder, name, ?value: string) =
        match value with
        | Some value -> do this.Attributes <- (name, Some value) :: this.Attributes
        | None -> ()
        this
    [<Extension>]
    static member inline attrBool(this: #NodeBuilder, name, ?value: bool) =
        match value with
        | None | Some true ->
            do this.Attributes <- (name, None) :: this.Attributes
        | Some false -> ()
        this
    [<Extension>]
    static member on(this: #NodeBuilder, name, ?handler: EventHandler) =
        match handler with
        | Some handler -> do this.Events <- (name, handler) :: this.Events
        | None -> ()
        this

[<Extension>]
type HTMLElementBuilderExtensions() =
    [<Extension>]
    static member inline id(this: #HTMLElementBuilder, ?value: string) =
        NodeBuilderExtensions.attrCond(this, "id", ?value = value)
    [<Extension>]
    static member inline class'(this: #HTMLElementBuilder, ?value: string) =
        NodeBuilderExtensions.attrCond(this, "class", ?value = value)
    [<Extension>]
    static member inline hidden(this: #HTMLElementBuilder, ?value: bool) =
        NodeBuilderExtensions.attrBool(this, "hidden", ?value = value)

[<Extension>]
type HTMLAnchorElementBuilderExtensions() =
    [<Extension>]
    static member inline href(this: #HTMLAnchorElementBuilder, ?value: string) =
        // Fable BUG https://github.com/fable-compiler/Fable/issues/3073
        // this is caising the exception on "npm start"
        //this.attrCond("href", ?value = value)
        // this is working
        NodeBuilderExtensions.attrCond(this, "href", ?value = value)

let inline element ctor tagName updateNode =
    ctor(
        (fun ctx -> ctx.elementsContext.AddElement(tagName) :> Node),
        updateNode)

// open type (why? -> We need always a new builder)
type Html =
    static member text<'s> text =
        let create (ctx: Context) =
            ctx.elementsContext.AddTextNode text :> Node
        let update (node: Node) =
            if node.textContent <> text then node.textContent <- text
        NodeBuilder(create, update)
    static member div = element HTMLElementBuilder "div" ignore
    static member span = element HTMLElementBuilder "span" ignore
    static member p = element HTMLElementBuilder "p" ignore
    static member button = element HTMLButtonElementBuilder "button" ignore
    static member a = element HTMLAnchorElementBuilder "a" ignore

    // TODO: Yield should work for strings

type VideBaseBuilder with
    member inline _.Yield(
        v: NodeBuilder)
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
        Html.text x { () }

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
