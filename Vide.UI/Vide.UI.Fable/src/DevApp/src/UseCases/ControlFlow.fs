module UseCases.ControlFlow

open Vide
open type Vide.Html

let ifElseWithForget =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count.Set(count.Value + 1)) {
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

        button.onclick(fun _ -> count.Set(count.Value + 1)) {
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
    button.id("dec").onclick(fun _ -> state.Set(state.Value - 1)) { "dec" }
    button.id("inc").onclick(fun _ -> state.Set(state.Value + 1)) { "inc" }
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

module MatchWithBranch =

    type Union =
        | View0 of string
        | View1 of int
        | View2

    let chooseView = vide {
        let! count = Vide.ofMutable 0
        button.id("dec").onclick(fun _ -> count.Set(count.Value - 1)) { "Previous View" }
        button.id("inc").onclick(fun _ -> count.Set(count.Value + 1)) { "Next View" }
        $" - View nr. {count.Value}"
        return count.Value
    }
    
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
            | View0 text ->
                B1Of3 <| vide {
                    let! x = Vide.preserveValue text
                    $"Case0: {x}"
                }
            | View1 num ->
                B2Of3 <| vide {
                    let! x = Vide.preserveValue num
                    $"Case1: {x}"
                }
            | View2 ->
                B3Of3 <| vide {
                    let! x = Vide.preserveValue ()
                    $"Case2: {x}"
                }
        }
