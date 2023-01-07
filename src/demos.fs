module Demos

open System
open Fable.Core
open Vide
open type Vide.Html

type Later<'a>(lazyPromise: unit -> JS.Promise<'a>) =
    member _.Start(onfulfilled: 'a -> unit, onrejected) =
        lazyPromise().``then``(onfulfilled, onrejected)
        |> ignore

// TODO: Docu
/// Produces cold promises.
/// Basically a forwarder to Fable's PromiseBuilder, except of Delay and Run.
type LaterBuilder() =
    let that = Promise.PromiseBuilder()

    member _.Bind(m, f) = that.Bind(m, f)
    member _.Combine(pa, pb) = that.Combine(pa, pb)
    member _.For(seq: _ seq, body) = that.For(seq, body)
    member _.For(p: JS.Promise<_>, f)= that.For(p, f)
    member _.While(guard, p) = that.While(guard, p)
    member _.Return(a) = that.Return(a)
    member _.ReturnFrom(p) = that.ReturnFrom(p)
    member _.Zero() = that.Zero()
    member _.TryFinally(p, compensation) = that.TryFinally(p, compensation)
    member _.TryWith(p, catchHandler) = that.TryWith(p, catchHandler)
    member _.Using(resource, binder) = that.Using(resource, binder)

    member _.MergeSources(a, b) = that.MergeSources(a, b)
    member _.MergeSources3(a, b, c) = that.MergeSources3(a, b, c)
    member _.MergeSources4(a, b, c, d) = that.MergeSources4(a, b, c, d)
    member _.MergeSources5(a, b, c, d, e) = that.MergeSources5(a, b, c, d, e)
    member _.MergeSources6(a, b, c, d, e, f) = that.MergeSources6(a, b, c, d, e, f)

    [<CustomOperation("andFor", IsLikeZip=true)>]
    member _.Merge(a, b, [<ProjectionParameter>] resultSelector) = that.Merge(a, b, resultSelector)

    // Needed for making promises lazy: ``` later { Promise.sleep 1000 } ```
    member _.Yield(p: JS.Promise<_>) = that.ReturnFrom(p)

    // Just leave the delayed func.
    member _.Delay(f: unit -> JS.Promise<'a>) = f

    member _.Run(p) = Later(p)

let later = LaterBuilder()

type BuilderOperations = | Clear

type ComputationState<'v> =
    {
        prom: Later<'v>
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
            m: Later<'v1>,
            f: 'v1 -> Vide<unit,'s2,'c>
        ) : Vide<unit, ComputationState<'v1> * 's2 option, 'c>
        =
        Vide <| fun s ctx ->
            let ms,fs =
                match s with
                | None ->
                    let result = ref None
                    do m.Start(
                        (fun res ->
                            result.Value <- Some res
                            ctx.EvaluateView()),
                        ignore)
                    let comp =
                        { 
                            prom = m
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
        let! res1 = later {
            do! Promise.sleep waitTimeInMs
            return 42
        }
        p { finishedMessage 1 res1 }

        p { loadingMessage 2 }
        let! res2 = later {
            do! Promise.sleep waitTimeInMs
            return 187
        }
        p { finishedMessage 2 res2 }
        
        p { loadingMessage 3 }
        do! later { Promise.sleep waitTimeInMs }

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
        "TODO: That doesn'a work right now"
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
