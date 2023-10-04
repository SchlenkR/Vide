module UseCases.Advanced

open Vide
open type Vide.Html

let directAccessToHtmlElementViaInitAndEval =
    vide {
        div.onEval(fun x -> x.node.className <- "bam")  {
            "I'm the OnEval div"
        }

        div.onInit(fun x -> x.node.className <- "bam2") {
            "I'm the OnInit div"
        }
    }

//let directAccessToHtmlElementViaComputation =
//    vide {
//        let x = div {
//            // TODO: This (specifying gen arg + internal cast) is not what I want...

//            let! htmlDivElement = Vide.node<HTMLDivElement>
//            $"...we can now access HTML element: {htmlDivElement.nodeName}"
//        }
//        x
//    }

let shouldCompile1 =
    // Both void and content builders should be able to yield
    // from inside of a content workflow
    vide {
        div {
            hr
            p
        }
    }
    
let shouldCompile2 =
    // Both void and content builders should be able to yield
    // from inside of a vide workflow
    vide {
        hr
        p
    }

// TODO: returning a node from inside { .. } to enable "let! node = ..."

let shouldCompile3 =
    vide {
        let! count = ofMutable {0}

        button.onclick(fun _ -> count.Set(count.Value + 1)) {
            $"Hit me! Count = {count.Value}"
        }

        if count.Value % 5 = 0 && count.Value <> 0 then
            let! valueString = Vide.preserveValue "Hello String"
            div.class'("the-message") { 
                $"You have the right to defend yourself! (string value {valueString})" 
            }
        else 
            elseForget

        // this must compile
        ()

        if count.Value <> 5 then
            let! valueInt = Vide.preserveValue 42
            p { $"not yet... with int value {valueInt}" }
        else
            elseForget
    }
