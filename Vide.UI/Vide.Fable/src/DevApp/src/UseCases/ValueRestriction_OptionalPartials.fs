namespace ValueRestriction_OptionalPartials

// see also: https://github.com/RonaldSchlenker/Vide/issues/3


//module OriginalIssue =

//    open Vide
//    open type Vide.Html

//    type Card =
//        static member Card
//            (
//                // it was super important to make this a function
//                // otherwise the header would "always win" and the footer would error out
//                content: Vide<_,_,_>,
//                ?header: Vide<_,_,_>,
//                ?footer: Vide<_,_,_>
//            ) =
//            vide {
//                div {
//                    match header with
//                    | Some header -> header
//                    | None -> elseForget
                    
//                    content
                    
//                    match footer with
//                    | Some footer -> footer
//                    | None -> elseForget
//                }
//            }

//    // it may be a little funky, but I think this is tolerable to work with and may even 
//    // promote a style which has bettter composition (if you're already taking functions, why not keep it there?)
//    let view = 
//        Card.Card(
//            vide { 
//                p { "Lorem ipsum dolor sit amet." } 
//            },
//            footer = vide { 
//                let! intState = Vide.ofMutable 1
//                p { $"Footer with number: {intState.Value}" } 
//            }
//        )


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
