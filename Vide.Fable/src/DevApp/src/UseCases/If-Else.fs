module DevApp.UseCases.IfElse

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
        else 
            Vide.elseForget

        // this must compile
        ()

        if count.Value <> 5 then
            let! valueInt = Vide.preserveValue 42
            p { $"not yet... with int value {valueInt}" }
        else
            Vide.elseForget
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
