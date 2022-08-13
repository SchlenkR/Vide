
[<AutoOpen>]
module Vide.Fable

open Vide
open Browser
open Browser.Types

let internal log (o: obj) =
    console.log(o)
    // ()

[<AutoOpen>]
module FableDomExtensions =
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

let vide = VideBuilder<Context>()

module Mutable =
    type MutableValue<'a>(init: 'a) =
        let mutable x = init
        member val EvaluateView = (fun () -> ()) with get,set
        member this.Value
            with get() = x
            and set(value) =
                x <- value
                this.EvaluateView()

    let value x =
        Vide <| fun s (c: Context) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x))
            // depending on how the update trigger process works, we have to set this every time (or not)
            do s.EvaluateView <- c.evaluateView
            s, Some s

[<AutoOpen>]
module Dom =

    type NodeBuilder(
        createNode: Context -> Node,
        updateNode: Node -> unit)
        =
        inherit VideBuilder<Context>()

        let mutable attributes = []
        let mutable events = []

        member _.Attributes = attributes
        member _.AddAttribute(name: string, value: string option) =
            do attributes <- (name, value) :: attributes

        member _.Events = events
        member _.AddEvent(name: string, handler: Event -> unit) =
            do events <- (name, handler) :: events

        static member ( ** ) (b: NodeBuilder, attr: (string * string option)) =
            b.AddAttribute(attr)

        member _.Run(
            childVide: Vide<unit,_,Context>)
            : Vide<unit,_,Context>
            =
            log "Run"
            let run (Vide childVide) =
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
                            // TODO: Attrs only on HTMLElement, not Node
                            let removedAttrs = oldAttributes |> except attributes
                            for name,_ in removedAttrs do
                                node.attributes.removeNamedItem(name) |> ignore
                            for name,value in attributes do
                                let attr = document.createAttribute(name)
                                do value |> Option.iter (fun value -> attr.value <- value)
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
            run childVide

    // attributes can be mixed in the builder
    module AttributeDefinitions =
        type IdAttribute = Id of string
        type ClassAttribute = Class of string
        type HiddenAttribute = Hidden
        type HRefAttribute = HRef of string

    // open this type to access lower case attr names
    type Attributes =
        static member id' = AttributeDefinitions.Id
        static member class' = AttributeDefinitions.Class
        static member hidden' = AttributeDefinitions.Hidden

    type EventHandler = Event -> unit

    module EventDefinitions =
        type OnClick = OnClick of EventHandler

    type Events =
        static member onclick = EventDefinitions.OnClick

    type HTMLElementBuilder(createNode, updateNode) =
        inherit NodeBuilder(createNode, updateNode)
        
        static member inline ( ++ ) (this: HTMLElementBuilder, EventDefinitions.OnClick handler) =
            this.AddEvent("onclick", handler); this
        
        static member inline ( ** ) (this: HTMLElementBuilder, AttributeDefinitions.Id value) =
            this.AddAttribute("id", Some value); this
        static member inline ( ** ) (this: HTMLElementBuilder, AttributeDefinitions.Class value) =
            this.AddAttribute("class", Some value); this
        static member inline ( ** ) (this: HTMLElementBuilder, AttributeDefinitions.Hidden _) =
            this.AddAttribute("class", None); this

    type HTMLAnchorElementBuilder(createNode, updateNode) =
        inherit NodeBuilder(createNode, updateNode)
        static member inline ( ** ) (this: HTMLAnchorElementBuilder, AttributeDefinitions.HRef value) =
            this.AddAttribute("href", Some value); this

    let inline element ctor tagName =
        ctor((fun ctx -> ctx.elementsContext.AddElement(tagName) :> Node), ignore)

    // open this type
    type Elements =
        static member text (html: string) =
            let createNode (ctx: Context) =
                ctx.elementsContext.AddTextNode html :> Node
            let update (node: Node) =
                if node.textContent <> html then
                    node.textContent <- html
            NodeBuilder(createNode, update)
        static member div with get () = element HTMLElementBuilder "div"
        static member p with get () = element HTMLElementBuilder "p"
        static member span with get () = element HTMLElementBuilder "span"
        static member button with get () = element HTMLElementBuilder "button"
        static member a with get () = element HTMLAnchorElementBuilder "a"


type FinalState<'s> = option<'s> * option<unit>

type VideBuilder<'c> with
    // TODO: Put yield in the appropriate builders
    member inline _.Yield(
        v: VideBuilder<Context>)
        : Vide<unit, FinalState<'s2>, Context>
        =
        log "Yield (VideBuilder)"
        let res = v { () }
        res
    member inline _.Yield(
        html: string)
        : Vide<unit, FinalState<_>, Context>
        =
        log "Yield (string)"
        Elements.text html { () }
