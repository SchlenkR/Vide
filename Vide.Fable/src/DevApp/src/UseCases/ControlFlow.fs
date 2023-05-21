module UseCases.ControlFlow

open Vide
open type Vide.Html

let ifElseWithForget =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        if count.Value % 5 = 0 && count.Value <> 0 then
            div.class'("the-message") { 
                $"You have the right to defend yourself!" 
            }
        else 
            elseForget
    }

// TODO: That is not compiling (anymore; which is ok - document this)
let ifElseWithPreserve =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        if count.Value % 5 = 0 && count.Value <> 0 then
            div.class'("the-message") {
                $"You have the right to defend yourself!" 
            }

            let! isAcknowledged = Vide.ofMutable false
            input.bind(isAcknowledged)
        else 
            elsePreserve
    }

////let yieldConditionalPreserve =
////    vide {
////        let! count = Vide.ofMutable 0
////        button.onclick(fun _ -> count += 1) {
////            $"Hit me! Count = {count.Value}"
////        }
////        // TODO: Why not committing to "is/else" - is that shortcut really good?
////        // Propably it's better to remove the possibility for "yieldConditionalPreserve" in Core
////        (count.Value = 5), p { "YOU DID IT!" }
////    }

let componentWithIntState = vide {
    let! state = Vide.ofMutable 0
    p { $"Int: {state.Value}" }
    button.id("dec").onclick(fun _ -> state -= 1) { "dec" }
    button.id("inc").onclick(fun _ -> state += 1) { "inc" }
}

let componentWithStringState = vide {
    let! state = Vide.ofMutable "Hello"
    p { $"String: {state.Value}" }
    input.bind(state)
}

let componentWithBooleanState = vide {
    let! state = Vide.ofMutable true
    p { $"Bool: {state.Value}" }
    input.bind(state)
}

let chooseView = vide {
    let! count = Vide.ofMutable 0
    button.id("dec").onclick(fun _ -> count -= 1) { "Previous View" }
    button.id("inc").onclick(fun _ -> count += 1) { "Next View" }
    $" - View nr. {count.Value}"
    return count.Value
}

module PartitionLikeActivePatterns =
    let choose = false,Vide.zero
    let caseMatch 
        cond
        (view: _ -> Vide<_,_,FableContext>)
        ((caseMatchedBefore,currView): bool * Vide<_,_,FableContext>)
        : (bool * Vide<_,_,FableContext>)
        =
        let resultingView =
            vide {
                currView
                match caseMatchedBefore,cond with
                | false,_
                | true, None -> elsePreserve // TODO: parametrize
                | true, Some x -> view x
            }
        (caseMatchedBefore || cond.IsSome), resultingView


    type Union =
        | View0 of string
        | View1 of int
        | View2

    let switchPartition =
        vide {
            let data = View0 "Test"

            choose
            |> caseMatch (match data with View0 text -> Some text | _ -> None) (fun text ->
                vide {
                    let! x = Vide.preserveValue text
                    p { $"Case0: {x}" }
                })
            |> caseMatch (match data with View1 num -> Some num | _ -> None) (fun num -> 
                vide {
                    let! x = Vide.preserveValue num
                    p { $"Case1: {x}" }
                })
            |> caseMatch (match data with View2 -> Some () | _ -> None) (fun () -> 
                vide {
                    let! x = Vide.preserveValue ()
                    p { $"Case2: {x}" }
                })
            |> snd
        }

module MatchWithChoice =

    // This will be hidden in "yield" builder methods, so the user
    // won't have to explicitly invoke this. Also: More Choices.
    let toVide c =
        vide {
            match c with Choice1Of3 v -> ensureVide v | _ -> elsePreserve
            match c with Choice2Of3 v -> ensureVide v | _ -> elsePreserve
            match c with Choice3Of3 v -> ensureVide v | _ -> elsePreserve
        }

    type Union =
        | View0 of string
        | View1 of int
        | View2

    let view =
        vide {
            let! data = vide {
                let! viewNr = chooseView
                return
                    match viewNr % 3 with
                    | 0 -> View0 "Test"
                    | 1 -> View1 42
                    | _ -> View2
            }

            match data with
            | View0 text -> Choice1Of3 (
                vide {
                    let! x = Vide.preserveValue text
                    p { $"Case0: {x}" }
                })
            | View1 num -> Choice2Of3 (
                vide {
                    let! x = Vide.preserveValue num
                    p { $"Case1: {x}" }
                })
            | View2 -> Choice3Of3 (
                vide {
                    let! x = Vide.preserveValue ()
                    p { $"Case2: {x}" }
                })
            |> toVide
        }

////module MatchWithBranch =

////    type Branch<'a,'b,'c,'d,'e> =
////        | B1 of 'a
////        | B2 of 'b
////        | B3 of 'c
////        | B4 of 'd
////        | B5 of 'e

////    // This will be hidden in "yield" builder methods, so the user
////    // won't have to explicitly invoke this. Also: More Choices.
////    let asBranch<'a,'b,'c,'d,'e> (c: Branch<'a,'b,'c,'d,'e>) =
////        vide {
////            match c with B1 v -> ensureVide v | _ -> elsePreserve
////            match c with B2 v -> ensureVide v | _ -> elsePreserve
////            match c with B3 v -> ensureVide v | _ -> elsePreserve
////            match c with B4 v -> ensureVide v | _ -> elsePreserve
////            match c with B5 v -> ensureVide v | _ -> elsePreserve
////        }

////    type Union =
////        | View0 of string
////        | View1 of int
////        | View2

////    let view =
////        vide {
////            let! data = vide {
////                let! viewNr = chooseView
////                return
////                    match viewNr % 3 with
////                    | 0 -> View0 "Test"
////                    | 1 -> View1 42
////                    | _ -> View2
////            }

////            match data with
////            | View0 text -> B1 (
////                vide {
////                    let! x = Vide.preserveValue text
////                    p { $"Case0: {x}" }
////                })
////            | View1 num -> B2 (
////                vide {
////                    let! x = Vide.preserveValue num
////                    p { $"Case1: {x}" }
////                })
////            | View2 -> B3 (
////                vide {
////                    let! x = Vide.preserveValue ()
////                    p { $"Case2: {x}" }
////                })
////            |> asBranch
////        }
