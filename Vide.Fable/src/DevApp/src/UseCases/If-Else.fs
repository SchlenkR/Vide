module DevApp.UseCases.IfElse

open System
open Browser.Types
open Vide
open type Vide.Html


let conditionalIfs =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        // TODO. Docu: The state can get lost because they are not compatible

        if count.Value = 5 || count.Value = 6 then
            let! valueString = Vide.preserveValue "Hello String"
            div.class'("the-message") { 
                $"You have the right to defend yourself! (string value {valueString})" 
            }
        // TODO:
        // else zero: instead of "zero", it could be controlled if component state
        // in the corresponding "if" will be preserved or cleared!
        else Vide.zero

        // this must compile
        ()

        if count.Value <> 5 then
            let! valueInt = Vide.preserveValue 42
            p { $"not yet... with int value {valueInt}" }
        else Vide.zero
    }

// TODO: That is not compiling (anymore; which is ok - document this)
let conditionalIfElse =
    vide {
        let! count = Vide.ofMutable 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        "if-else cannot work like that"

        ////// TODO: That should not be used at all? And: That this seems to work
        ////// is only an edge case, because state has same type
        ////if count.Value = 5 then
        ////    div.class'("the-message") { 
        ////        $"You have the right to defend yourself!" 
        ////    }
        ////else
        ////    p { $"not yet..." }
    }

let simpleFor =
    vide {
        for x in 0..5 do
            div.class'("card") { $"I'm element no. {x}" }
    }

let statelessFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Vide.ofMutable []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items :=  []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.class'("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Vide.ofMutable []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items := []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.class'("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }

                let! count = Vide.ofMutable 0
                button.onclick(fun _ -> count -= 1) { "dec" }
                $"{count.Value}  "
                button.onclick(fun _ -> count += 1) { "inc" }
        }
    }
