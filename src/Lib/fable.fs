[<AutoOpen>]
module Vide.Fable

open Browser
open Browser.Types
open Vide
open System

type NodeList with 
    member this.ToSeq() = seq { for i in 0 .. this.length-1 do this.Item i }
    member this.ToList() = this.ToSeq() |> Seq.toList

module NodeExt =
    let displayString (node: Node) =
        let idOrDefault = try node.attributes.getNamedItem("id").value with _ -> "--"
        $"<{node.nodeName} id='{idOrDefault}'>"

type Controller =
    {
        node: Node
        mutable evaluateView: unit -> unit
        mutable elementsContext: ElementsContext
    }

and ElementsContext(parent: Node) =
    let mutable keptChildren = []
    let memory child =
        keptChildren <- (child :> Node) :: keptChildren
        child
    let append child =
        do parent.appendChild(child) |> ignore
        child
    member _.DummyElement() =
        document.createElement "span"
    member _.AddElement<'n when 'n :> HTMLElement>(tagName: string) =
        document.createElement tagName |> memory |> append :?> 'n
    member _.AddText(text: string) =
        document.createTextNode text |> memory |> append
    member _.KeepChild(child: Node) =
        child |> memory |> ignore
    member _.GetObsoleteChildren() =
        let childNodes = parent.childNodes.ToList()
        childNodes |> List.except keptChildren

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
        Vide <| fun s (c: Controller) ->
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

type Modifier<'n> = 'n -> unit
type NodeBuilderState<'n,'s when 'n :> Node> = option<'n> * option<'s>
type NodeCheckResult = Keep | DiscardAndCreateNew

type NodeBuilder<'n when 'n :> Node>(createNode, checkOrUpdateNode) =
    inherit VideBuilder()
    let mutable modifiers: Modifier<'n> list = []
    let mutable initOnlyModifiers: Modifier<'n> list = []
    member this.AddModifier(m: Modifier<'n>) =
        do modifiers <- m :: modifiers
        this
    member this.AddInitOnlyModifier(m: Modifier<'n>) =
        do initOnlyModifiers <- m :: initOnlyModifiers
        this
    member this.Run
        (Vide childVide: Vide<unit,'fs,Controller>)
        : Vide<'n, NodeBuilderState<'n,'fs>, Controller>
        =
        let runModifiers modifiers node =
            for x in modifiers do
                x node
        Vide <| fun s (controller: Controller) ->
            let s,cs = separateStatePair s
            let node,cs =
                match s with
                | None ->
                    let newNode,s = createNode controller,cs
                    do newNode |> runModifiers initOnlyModifiers
                    newNode,s
                | Some node ->
                    match checkOrUpdateNode node with
                    | Keep ->
                        controller.elementsContext.KeepChild(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        createNode controller,None
            do runModifiers modifiers node
            let childController =
                {
                    node = node
                    evaluateView = controller.evaluateView
                    elementsContext = ElementsContext(node)
                }
            let cv,cs = childVide cs childController
            for x in childController.elementsContext.GetObsoleteChildren() do
                node.removeChild(x) |> ignore
                // we don'tneed this? Weak enough?
                // events.RemoveListener(node)
            node, Some (Some node, cs)

// we always use EmitBuilder and "map ignore" the result in yield or use it in bind
////type DiscardNodeBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
////    inherit NodeBaseBuilder<'n>(newNode, checkOrUpdateNode)
////    member this.Run
////        (
////            childVide: Vide<unit,'fs,Context>
////        ) : Vide<unit, NodeBuilderState<'fs, 'n>, Context>
////        =
////        this.SyncNode(childVide) |> map ignore

type RootBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
    inherit NodeBuilder<'n>(newNode, checkOrUpdateNode)
    member inline _.Yield
        (x: Vide<unit,'s,Controller>)
        : Vide<unit,'s,Controller>
        =
        x

type HTMLElementBuilder<'n when 'n :> HTMLElement>(elemName: string) =
    inherit NodeBuilder<'n>(
        (fun controller -> controller.elementsContext.AddElement<'n>(elemName)),
        (fun node ->
            match node.nodeName.Equals(elemName, StringComparison.OrdinalIgnoreCase) with
            | true -> Keep
            | false ->
                // TODO:
                console.log($"TODO: if/else detection? Expected node name: {elemName}, but was: {node.nodeName}")
                DiscardAndCreateNew
        )        
    )

// open type (why? -> We need always a new builder on property access)
module internal HtmlBase =
    // TODO: This is something special
    let inline nothing () =
        NodeBuilder(
            (fun controller -> controller.elementsContext.DummyElement()),
            (fun node -> Keep))
    // TODO: This is something special
    let inline text text =
        NodeBuilder(
            (fun controller -> controller.elementsContext.AddText(text)),
            (fun node ->
                if typeof<Text>.IsInstanceOfType(node) then
                    if node.textContent <> text then node.textContent <- text
                    Keep
                else
                    DiscardAndCreateNew
            ))

type VideBuilder with
    member inline this.Bind
        (
            x: NodeBuilder<'n>,
            f: 'n -> Vide<'v,'s1,Controller>
        ) : Vide<'v,NodeBuilderState<'n,unit> option * 's1 option,Controller>
        =
        let v = x { () }
        this.Bind(v, f)
    member inline _.Yield<'n,'s,'c when 'n :> Node>
        (v: Vide<'n,'s,'c>)
        : Vide<unit,'s,'c>
        =
        v |> map ignore
    member inline _.Yield
        (x: NodeBuilder<'n>)
        : Vide<unit, NodeBuilderState<'n,unit> ,Controller>
        =
        x { () } |> map ignore
    member inline _.Yield
        (x: string)
        : Vide<unit, NodeBuilderState<Text,unit> ,Controller>
        =
        HtmlBase.text x { () } |> map ignore

let inline startApp (holder: #Node) (v: Vide<_,'s,Controller>) onEvaluated =
    let controller =
        {
            node = holder
            evaluateView = fun () -> ()
            elementsContext = ElementsContext(holder)
        }
    let videMachine =
        VideMachine(
            None,
            controller,
            RootBuilder((fun _ -> holder), fun _ -> Keep) { v },
            onEvaluated)
    do controller.evaluateView <- videMachine.Eval
    videMachine

let vide = VideBuilder()
