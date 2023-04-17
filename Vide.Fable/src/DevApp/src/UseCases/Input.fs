module DevApp.UseCases.Input

open System
open Browser.Types
open Vide
open type Vide.Html

let textInputReturnsValue = 
    vide {
        // TODO: Docu - wie ändert man das PropertyChangedTrigger-Verhalten
        let! enteredValue = input.type'("text").oninput()
        div {
            $"You say: {enteredValue.TextValue}"
        }
    }
    
let textInputEvent = 
    vide {
        let! enteredText = Vide.ofMutable ""

        div {
            $"You say: %s{enteredText.Value}"
        }

        // TODO: Docu - Default-Verhalten von input
        input.type'("text").oninput(fun e -> enteredText.Value <- e.node.value)
    }
    
let textInputComponent = 
    vide {
        let! enteredText = div {
            let! enteredValue = input.type'("text").oninput()
            return enteredValue.TextValue
        }

        div {
            $"You say: {enteredText}"
        }
    }
