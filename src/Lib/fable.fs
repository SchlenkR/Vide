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

type FableContext
    (
        parent: Node, 
        evaluateView: unit -> unit
    ) =
    inherit VideContext(evaluateView)
    let mutable keptChildren = []
    let memory child =
        keptChildren <- (child :> Node) :: keptChildren
        child
    let append child =
        do parent.appendChild(child) |> ignore
        child
    member _.Parent = parent
    member _.AddElement<'n when 'n :> HTMLElement>(tagName: string) =
        document.createElement tagName |> memory |> append :?> 'n
    member _.AddText(text: string) =
        document.createTextNode text |> memory |> append
    member _.KeepChild(child: Node) =
        child |> memory |> ignore
    member _.GetObsoleteChildren() =
        let childNodes = parent.childNodes.ToList()
        childNodes |> List.except keptChildren

type Modifier<'n> = 'n -> unit
type NodeBuilderState<'n,'s> = option<'n> * option<'s>
type NodeCheckResult = Keep | DiscardAndCreateNew

type NodeBuilder<'n when 'n :> Node>
    (
        createNode: FableContext -> 'n,
        checkOrUpdateNode: 'n -> NodeCheckResult
    ) =
    inherit VideBuilder()
    let mutable modifiers: Modifier<'n> list = []
    let mutable initOnlyModifiers: Modifier<'n> list = []
    member this.OnEval(m: Modifier<'n>) =
        do modifiers <- m :: modifiers
        this
    member this.OnInit(m: Modifier<'n>) =
        do initOnlyModifiers <- m :: initOnlyModifiers
        this
    member _.Run
        (Vide childVide: Vide<'v,'fs,FableContext>)
        : Vide<'v, NodeBuilderState<'n,'fs>, FableContext>
        =
        Vide <| fun s (ctx: FableContext) ->
            Debug.print "RUN:NodeBuilder"
            let inline runModifiers modifiers node =
                for m in modifiers do m node
            let s,cs = separateStatePair s
            let node,cs =
                match s with
                | None ->
                    let newNode,s = createNode ctx,cs
                    do runModifiers initOnlyModifiers newNode
                    newNode,s
                | Some node ->
                    match checkOrUpdateNode node with
                    | Keep ->
                        ctx.KeepChild(node)
                        node,cs
                    | DiscardAndCreateNew ->
                        createNode ctx,None
            do runModifiers modifiers node
            let childCtx = FableContext(node, ctx.EvaluateView)
            let cv,cs = childVide cs childCtx
            for x in childCtx.GetObsoleteChildren() do
                node.removeChild(x) |> ignore
                // we don'tneed this? Weak enough?
                // events.RemoveListener(node)
            cv, Some (Some node, cs)

type HTMLElementBuilder<'n when 'n :> HTMLElement>(elemName: string) =
    inherit NodeBuilder<'n>(
        (fun ctx -> ctx.AddElement<'n>(elemName)),
        (fun node ->
            match node.nodeName.Equals(elemName, StringComparison.OrdinalIgnoreCase) with
            | true -> Keep
            | false ->
                // TODO:
                console.log($"TODO: if/else detection? Expected node name: {elemName}, but was: {node.nodeName}")
                DiscardAndCreateNew
        )        
    )

module internal HtmlBase =
    // TODO: This is something special
    let inline nothing () =
        NodeBuilder(
            (fun ctx -> ctx.AddElement "span"),
            (fun node -> Keep))
    // TODO: This is something special
    let inline text text =
        NodeBuilder(
            (fun ctx -> ctx.AddText(text)),
            (fun node ->
                if typeof<Text>.IsInstanceOfType(node) then
                    if node.textContent <> text then
                        node.textContent <- text
                    Keep
                else
                    DiscardAndCreateNew
            ))

type VideBuilder with
    member inline _.Yield<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<unit,'s,'c>
        =
        Debug.print "YIELD Vide"
        v |> map ignore
    /// This allows constructs like:
    ///     div
    /// What is already allowed is (because of Run):
    ///     div { nothing }
    member inline _.Yield
        (nb: NodeBuilder<'n>)
        : Vide<unit, NodeBuilderState<'n,unit>, FableContext>
        =
        Debug.print "YIELD NodeBuilder"
        nb { () } |> map ignore
    member inline _.Yield
        (s: string)
        : Vide<unit, NodeBuilderState<Text,unit>, FableContext>
        =
        Debug.print "YIELD string"
        HtmlBase.text s { () } |> map ignore

module App =
    type RootBuilder<'n when 'n :> Node>(newNode, checkOrUpdateNode) =
        inherit NodeBuilder<'n>(newNode, checkOrUpdateNode)
        member inline _.Yield
            (x: Vide<unit,'s,FableContext>)
            : Vide<unit,'s,FableContext>
            =
            x
    
    let inline start (holder: #Node) (v: Vide<_,'s,FableContext>) onEvaluated =
        let ctx = FableContext(holder, (fun () -> ()))
        let videMachine =
            VideMachine(
                None,
                ctx,
                RootBuilder((fun _ -> holder), fun _ -> Keep) { v },
                onEvaluated)
        do ctx.EvaluateView <- videMachine.EvaluateView
        videMachine
