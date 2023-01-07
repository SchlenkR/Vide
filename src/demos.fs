module Demos

open Vide
open type Vide.Html

type BuilderOperations = | Clear

type ComputationState<'v> =
    {
        prom: Fable.Core.JS.Promise<'v>
        result: Ref<'v option>
    }

type VideBuilder with
    member inline _.Yield
        (op: BuilderOperations) 
        : Vide<unit,unit,FableContext>
        =
        Vide <| fun s ctx ->
            match op with
            | Clear -> ctx.Parent.textContent <- ""
            (),None

    // TODO: returning values

    // If Bind is marked inline, runtime errors can occur when using
    // "do!" or "let! _ =" (seems to be a bug in fable compiler).
    member _.Bind<'v1,'s2,'c when 'c :> VideContext>
        (
            m: unit -> Fable.Core.JS.Promise<'v1>,
            f: 'v1 -> Vide<unit,'s2,'c>
        ) : Vide<unit, ComputationState<'v1> * 's2 option, 'c>
        =
        Vide <| fun s ctx ->
            let ms,fs =
                match s with
                | None ->
                    let result = ref None
                    let prom = m().``then``(fun res ->
                        do 
                            result.Value <- Some res
                            ctx.EvaluateView()
                        res
                    )
                    let comp =
                        { 
                            prom = prom
                            result = result
                        }
                    comp,None
                | Some (comp,fs) ->
                    match comp.result.Value with
                    | Some mres ->
                        let (Vide f) = f mres
                        let fv,fs = f fs ctx
                        comp,fs
                    | None -> comp,fs
            (), Some (ms,fs)

let asyncHelloWorld =
    vide {
        let waitTimeInMs = 2000
        let loadingMessage phase = $"loading phase %d{phase}... please wait {float waitTimeInMs / 1000.0} seconds"
        let finishedMessage phase res = $"Phase %d{phase}.finished with result = {res}"

        p { loadingMessage 1 }
        let! res1 = fun () -> promise {
            do! Promise.sleep waitTimeInMs
            return 42
        }
        p { finishedMessage 1 res1 }

        p { loadingMessage 2 }
        let! res2 = fun () -> promise {
            do! Promise.sleep waitTimeInMs
            return 187
        }
        p { finishedMessage 2 res2 }
        
        p { loadingMessage 3 }
        do! fun () -> Promise.sleep waitTimeInMs

        Clear
        p { "--- END ---" }
    }

////let asyncTrigger =
////    vide {
////        let waitTimeInMs = 2000
////        let loadingMessage phase = $"loading phase %d{phase}... please wait {float waitTimeInMs / 1000.0} seconds"
////        let finishedMessage phase res = $"Phase %d{phase}.finished with result = {res}"

////        p { loadingMessage 1 }
        
////        let! res1 = fun () -> promise {
////            do! Promise.sleep waitTimeInMs
////            return 42
////        }

////        //clear
////        p { finishedMessage 1 res1 }
////        p { loadingMessage 2 }
        
////        let! res2 = fun () -> promise {
////            do! Promise.sleep waitTimeInMs
////            return 187
////        }

////        //clear
////        p { finishedMessage 2 res2 }
////        p { "--- END ---" }
////    }

let helloWorld =
    vide { "Hello World" }

let counter =
    vide {
        let! count = Mutable.ofValue 0

        div { $"Count = {count.Value}" }
        button.onclick(fun _ -> count -= 1) { "dec" }
        button.onclick(fun _ -> count += 1) { "inc" }
    }

let conditionalAttributes =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }
        "TODO: That doesn't work right now"
        div.className("the-message") {
            nothing
            //span.hidden(count.Value <> 5) {
            //    "You have the right to defend yourself!"
            //}
        }
    }

let conditionalIfs =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        if count.Value = 5 || count.Value = 6 then
            let! valueString = preserve "Hello String"
            div.className("the-message") { 
                $"You have the right to defend yourself! (string value {valueString})" 
            }
        if count.Value <> 5 then
            let! valueInt = preserve 42
            p { $"not yet... with int value {valueInt}" }
    }

// TODO: That is not compiling (anymore; which is ok - document this)
let conditionalIfElse =
    vide {
        let! count = Mutable.ofValue 0

        button.onclick(fun _ -> count += 1) {
            $"Hit me! Count = {count.Value}"
        }

        "if-else cannot work like that"

        ////// TODO: That should not be used at all? And: That this seems to work
        ////// is only an edge case, because state has same type
        ////if count.Value = 5 then
        ////    div.className("the-message") { 
        ////        $"You have the right to defend yourself!" 
        ////    }
        ////else
        ////    p { $"not yet..." }
    }

let simpleFor =
    vide {
        for x in 0..5 do
            div.className("card") { $"I'm element no. {x}" }
    }

let statelessFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Mutable.ofValue []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items :=  []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.className("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }
        }
    }

let statefulFor =
    let nextNum() = System.Random().Next(10000)
    vide {
        let! items = Mutable.ofValue []
        let add1 _ = items := items.Value @ [nextNum()]
        let add100 _ = items := items.Value @ [ for _ in 0..100 do nextNum() ]
        let removeAll _ = items := []

        button.onclick(add1) { "Add One" }
        button.onclick(add100) { "Add 100" }
        button.onclick(removeAll) { "Remove All" }
        
        for x in items.Value do
            div.className("card") {
                let removeMe _ = items := items.Value |> List.except [x]
                button.onclick(removeMe) { $"Remove {x}" }

                let! count = Mutable.ofValue 0
                button.onclick(fun _ -> count -= 1) { "dec" }
                $"{count.Value}  "
                button.onclick(fun _ -> count += 1) { "inc" }
        }
    }

// TODO: If that's not a function, we will have som val restr issues. Examine those!
let visualComponentReturningValues () =
    let visualCounter =
        vide {
            let! count = Mutable.ofValue 0

            button.onclick(fun _ -> count -= 1) { "dec" }
            button.onclick(fun _ -> count += 1) { "inc" }

            return count.Value
        }

    vide {
        let! count = visualCounter
        p { $"COUNT = {count}"}
    }

let directAccessToHtmlElement =
    vide {
        div.OnEval(fun x -> x.className <- "bam")  {
            "I'm the OnEval div"
        }

        div.OnInit (fun x -> x.className <- "bam2") {
            "I'm the OnInit div"
        }
    }
