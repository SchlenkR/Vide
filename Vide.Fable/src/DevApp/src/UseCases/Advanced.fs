module DevApp.UseCases.Advanced

open Vide
open type Vide.Html

let directAccessToHtmlElementViaInitAndEval =
    vide {
        div.OnEval(fun x -> x.node.className <- "bam")  {
            "I'm the OnEval div"
        }

        div.OnInit(fun x -> x.node.className <- "bam2") {
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

