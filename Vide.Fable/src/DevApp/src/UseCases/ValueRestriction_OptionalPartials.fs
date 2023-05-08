namespace ValueRestriction_OptionalPartials

// see also: https://github.com/RonaldSchlenker/Vide/issues/3

module Alt_With_Turning_Into_Functions =

    open Vide
    open type Vide.Html

    let Card
        (
            content: Vide<_,_,_>,
            footerContent: Vide<_,_,_> option
        ) =
        vide {
            div { content }
        
            match footerContent with
            | Some footerContent -> header { footerContent }
            | None -> elseForget
        }

    let cards() = vide {
        Card(
            vide {
                let! counter = Vide.ofMutable 0
                $"The current count is {counter.Value} :)"
            },
            Some (vide { "Footer is here" })
        )

        Card(
            vide { "This is just another Usage" },
            None
        )
    }

    let view() = vide { article { main { cards() } } }


module Alt_With_Specifying_GenArgs =

    open Vide
    open type Vide.Html

    let Card<'vc,'sc,'sf>
        (
            content: Vide<'vc,'sc,_>,
            footerContent: Vide<_,'sf,_> option
        ) =
        vide {
            div { content }
        
            match footerContent with
            | Some footerContent -> header { footerContent }
            | None -> elseForget
        }

    let cards = vide {
        Card(
            vide {
                let! counter = Vide.ofMutable 0
                $"The current count is {counter.Value} :)"
            },
            Some (vide { "Footer is here" })
        )

        Card<_,_,unit>(
            vide { "This is just another Usage" },
            None
        )
    }

    let view = vide { article { main { cards } } }


module Alt_With_VideNone =

    open Vide
    open type Vide.Html

    let Card
        (
            content: Vide<_,_,_>,
            footerContent: Vide<_,_,_> option
        ) =
        vide {
            div { content }
        
            match footerContent with
            | Some footerContent -> header { footerContent }
            | None -> elseForget
        }

    let videNone : Vide<unit,unit,_> option = None

    let cards = vide {
        Card(
            vide {
                let! counter = Vide.ofMutable 0
                $"The current count is {counter.Value} :)"
            },
            Some (vide { "Footer is here" })
        )

        Card(
            vide { "This is just another Usage" },
            videNone
        )
    }

    let view = vide { article { main { cards } } }


module Alt_With_ArgsAsFunctions =

    open Vide
    open type Vide.Html

    type Card =
        static member Card
            (
                // it was super important to make this a function
                // otherwise the header would "always win" and the footer would error out
                content: Vide<_, _, _>,
                ?header: Vide<_, _, _>,
                ?footer: Vide<_, _, _>
            ) =
            vide {
                div {
                    match header with
                    | Some header -> header
                    | None -> elseForget
                    
                    content
                    
                    match footer with
                    | Some footer -> footer
                    | None -> elseForget
                }
            }

    // it may be a little funky, but I think this is tolerable to work with and may even 
    // promote a style which has bettter composition (if you're already taking functions, why not keep it there?)
    let view = 
        Card.Card(
            vide { 
                p { "Lorem ipsum dolor sit amet." } 
            },
            header = vide {
                let! listState = Vide.ofMutable [1;2;3]
                p { $"Header with elements: {listState.Value}" } 
            },
            footer = vide { 
                let! intState = Vide.ofMutable 1
                p { $"Footer with number: {intState.Value}" } 
            }
        )

module Alt_With_ArgsAsFunctions2 =

    open Vide
    open type Vide.Html

    type MaybeVide<'v,'s,'c> = unit -> Vide<'v,'s,'c>
    let provide (v: Vide<_,_,_>) = fun () -> v

    type Card =
        static member Card
            (
                // it was super important to make this a function
                // otherwise the header would "always win" and the footer would error out
                content: Vide<_,_,_>,
                ?header: MaybeVide<_,_,_>,
                ?footer: MaybeVide<_,_,_>
            ) =
            vide {
                div {
                    match header with
                    | Some header -> Html.header{ header () }
                    | None -> elseForget
                    
                    content
                    
                    match footer with
                    | Some footer -> Html.footer { footer () }
                    | None -> elseForget
                }
            }

    let view = 
        Card.Card(
            vide {
                p {
                    "Lorem ipsum dolor sit amet."
                }
            },
            header = (provide <| vide { header { h3 { "Header" } } }),
            footer = (provide <| vide { footer { a.href ("#") { "Read more]" } } })
        )

module Alt_With_ArgsAsFunctions3 =

    open Browser.Types
    open Vide
    open type Vide.Html
    
    //type DelayedComponentRetCnBaseBuilder<'n,'c
    //        when 'n : equality
    //        and 'c :> NodeContext<'n> 
    //    > () =
    //    inherit VideBaseBuilder()
    //    member _.Return(x: 'v) = BuilderBricks.return'<'v,'c>(x)
    //    member _.Delay(f) = f
    //    member _.Combine(a, b) = BuilderBricks.combine(a, b)
    //    member _.For(seq, body) = BuilderBricks.for'(seq, body)

    //    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    //    member _.Yield(b: RenderPotC0BaseBuilder<_,_,_,_>) = b {()}
    //    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    //    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    //    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    //    member _.Yield(v) = BuilderBricks.yieldVide(v)
    //    member _.Yield(v) = BuilderBricks.yieldConditionalVide(v)
    //    member _.Yield(op) = BuilderBricks.yieldBuilderOp<'n,'c>(op)
    //    member _.Yield(op) = BuilderBricks.yieldText<'n,'c>(op)
    
    //    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    //    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    //    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    //    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    //    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

    //type DelayedComponentRetCnBuilder() =
    //    inherit DelayedComponentRetCnBaseBuilder<Node,FableContext>()

    //let videDelayed = DelayedComponentRetCnBuilder()

    //let test =
    //    videDelayed {
    //        p { "Whats up?" }
    //    }


    type MaybeVide<'v,'s,'c> = unit -> Vide<'v,'s,'c>
    let provide (v: Vide<_,_,_>) = fun () -> v

    type Card =
        static member Card
            (
                // it was super important to make this a function
                // otherwise the header would "always win" and the footer would error out
                content: Vide<_,_,_>,
                ?header: unit -> Vide<_,_,_>,
                ?footer: unit -> Vide<_,_,_>
            ) =
            vide {
                div {
                    match header with
                    | Some header -> Html.header { header() }
                    | None -> elseForget
                    
                    content
                    
                    match footer with
                    | Some footer -> Html.footer { footer() }
                    | None -> elseForget
                }
            }

    //let delayedFooter = videDelayed { footer { a.href ("#") { "Read more]" } } }

    let view = 
        Card.Card(
            vide {
                p {
                    "Lorem ipsum dolor sit amet."
                }
            },
            header = (fun () -> vide { header { h3 { "Header" } } }),
            footer = (fun () -> vide { footer { a.href ("#") { "Read more]" } } } )
        )
