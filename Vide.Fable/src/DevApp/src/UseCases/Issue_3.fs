namespace Issues

module Issue_2 =

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
            | None -> Vide.elseForget
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


module Issue_2a =

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
            | None -> Vide.elseForget
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


module Issue_2b =

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
        | None -> Vide.elseForget
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


//module Issue_2b =

//    open Vide
//    open type Vide.Html

//    let Card<'vc,'sc,'sf>
//        (
//            content: Vide<'vc,'sc,_>,
//            footerContent: Vide<_,'sf,_>
//        ) =
//        vide {
//            div { content }
        
//            match footerContent with
//            | Some footerContent -> header { footerContent }
//            | None -> Vide.elseForget
//        }


//    let cards = vide {
//        Card(
//            vide {
//                let! counter = Vide.ofMutable 0
//                $"The current count is {counter.Value} :)"
//            },
//            vide { "Footer is here" }
//        )

//        Card<_,_,unit>(
//            vide { "This is just another Usage" },
//            Vide.zero
//        )
//    }

//    let view = vide { article { main { cards } } }
