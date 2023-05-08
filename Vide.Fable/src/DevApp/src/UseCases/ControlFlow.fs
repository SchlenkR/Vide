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

let yieldConditionalPreserve =
    vide {
        let! count = Vide.ofMutable 0
        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        // TODO: Why not committing to "is/else" - is that shortcut really good?
        // Propably it's better to remove the possibility for "yieldConditionalPreserve" in Core
        (count.Value = 5), p { "YOU DID IT!" }
    }

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

let switchCase =
    vide {
        let! viewNr = chooseView

        switch id
        |> case (viewNr = 0) componentWithBooleanState
        |> caseForget (viewNr = 1) componentWithIntState
        |> case (viewNr = 2) componentWithStringState
    }

let switchCaseWithDefault =
    vide {
        let! viewNr = chooseView

        switch (fun x -> x = viewNr)
        |> case 0 componentWithBooleanState
        |> caseForget 1 componentWithIntState
        |> case 2 componentWithStringState
        |> caseDefault (div { "Nothing to show - this is the default case." })
    }

