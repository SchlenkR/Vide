module UseCases.For

open Vide
open type Vide.Html

let simpleFor =
    vide {
        for x in For.selfKeyed [0..5] do
            div.class'("card") { $"I'm element no. {x}" }
    }

let statelessFor =
    vide {
        let! items = Vide.ofMutable []
        let nextNum() = 0 :: items.Value |> List.max |> (+) 1
        let add1 _ = items.Value @ [nextNum()] |> items.Set
        let add100 _ = items.Value @ [ for _ in 0..100 do nextNum() ] |> items.Set
        let removeAll _ = items.Set []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }

        for x in For.selfKeyed items.Value do
            div.class'("card") {
                let removeMe _ = items.Value |> List.except [x] |> items.Set
                button.onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    vide {
        let! items = Vide.ofMutable []
        let nextNum() = 0 :: items.Value |> List.max |> (+) 1
        let add1 _ = items.Set <| items.Value @ [nextNum()]
        let add100 _ = items.Set <| items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items.Set <| []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value |> For.selfKeyed do
            div.class'("card") {
                let removeMe _ = 
                    printfn $"Removing element with ID {x} ..."
                    items.Value |> List.except [x] |> items.Set
                button.onclick(removeMe) { $"Remove {x}" }

                let! count = Vide.ofMutable 0
                button.onclick(fun _ -> count.Set(count.Value - 1)) { "dec" }
                $"{count.Value}  "
                button.onclick(fun _ -> count.Set(count.Value + 1)) { "inc" }
        }
    }
