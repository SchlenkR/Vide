module DevApp.UseCases.Components

open Vide
open type Vide.Html


// TODO: If that's not a function, we will have som val restr issues. Examine those!
let visualComponentReturningValues =
    let visualCounter =
        vide {
            let! count = Vide.ofMutable 0

            button.onclick(fun _ -> count -= 1) { "dec" }
            button.onclick(fun _ -> count += 1) { "inc" }

            return count.Value
        }

    vide {
        let! count = visualCounter
        p { $"COUNT = {count}"}
    }
