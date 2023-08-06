module UseCases.GettingStarted

open Vide
open type Vide.Html

// TODO: demo for async + clear

let helloWorld =
    vide { "Hello World" }

let counter =
    vide {
        let! count = Vide.ofMutable 0
        let v : int = count

        span.id("tmp") { "HELLO" }

        div {
            $"Count = "
            span.id("result") { $"{count.Value}"  }
        }
        button.id("dec").onclick(fun _ -> count -= 1) { "dec" }
        button.id("inc").onclick(fun _ -> count += 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Vide.ofMutable 0

        button.id("hitMe").onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }
        div.class'("the-message") {
            span.id("result").hidden(count.Value <> 5) {
                "You have the right to defend yourself!"
            }
        }
    }
