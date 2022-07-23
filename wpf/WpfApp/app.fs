// TODOs:
//  - Don't calc the whole tree when triggering Update
//  - first class task/async support (in gen)
//  - implement "for" in ChildBuilder
//  - hide all the crazy generic type signatures

module LocSta.App

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open LocSta.Core

type App(appElement: IAddChild, triggerUpdate: App -> FrameworkElement list) as this =
    do
        for elem in triggerUpdate this do
            appElement.AddChild elem
    member this.TriggerUpdate() =
        printfn $">>>> Trigger update"
        let element = triggerUpdate this
        // TODO: Sync returned element(s) with current
        ()
    static member Run(window: Window, view) =
        let sp = StackPanel()
        window.Content <- sp
        App(sp, view |> Gen.toEvaluable)


type AppGen<'v,'s> = Gen<'v,'s,App>
type RTState = obj
type RTAppGen<'v> = AppGen<'v,RTState>

let toBoxedGen
    (Gen x: Gen<'v,'s,'r>)
    : Gen<'v,RTState,'r>
    =
    printfn "toBoxedGen"
    fun s r ->
        let s =
            match s with
            | None -> None
            | Some (s: RTState) ->
                try Some (s :?> 's)
                with _ -> None
        let xv,xs = x s r
        xv, (xs :> obj)
    |> Gen

// TODO: Could it be that we neet "toRTAppGen" only in bind?
// TODO: Generalize (App, so that this can be used in any context / framework)
type ViewBuilder<'ret>(run: RTAppGen<FrameworkElement list> -> 'ret) =

    member _.Bind(
        m: AppGen<'v1, 's1>,
        f: 'v1 -> AppGen<'v2, 's2>)
        : RTAppGen<'v2>
        =
        printfn "Bind"
        Gen.bind m f |> toBoxedGen
    
    member _.Yield(
        x: AppGen<'v,'s>)
        : RTAppGen<FrameworkElement list>
        =
        printfn "Yield (single)"
        toBoxedGen x |> Gen.map (fun xv -> [xv :> FrameworkElement])

    member _.Yield(
        x: AppGen<'v list, 's>)
        : RTAppGen<FrameworkElement list>
        =
        printfn "Yield (many)"
        toBoxedGen x |> Gen.map (List.map (fun x -> x :> FrameworkElement))
    
    member _.Delay(
        f: unit -> RTAppGen<FrameworkElement list>)
        : RTAppGen<FrameworkElement list>
        =
        printfn "Delay"
        f()

    member _.Combine(
        a: RTAppGen<FrameworkElement list>,
        b: RTAppGen<FrameworkElement list>)
        : RTAppGen<FrameworkElement list>
        =
        printfn "Combine"
        gen {
            let! aNodes = a
            let! bNodes = b
            return List.append aNodes bNodes
        }
        |> toBoxedGen

    member this.For(
        s: seq<'a>,
        body: 'a -> RTAppGen<FrameworkElement list>)
        : RTAppGen<FrameworkElement list>
        =
        printfn "For"
        s
        |> Seq.map body
        |> Seq.fold (fun curr next -> this.Combine(curr, next)) (this.Zero())

    member _.Zero()
        : RTAppGen<FrameworkElement list>
        =
        // 's: same reason as in Combine
        printfn "Zero"
        Gen.ofValue [] |> toBoxedGen

    member _.Run(children) : 'ret =
        printfn "Run"
        run children

let pov = ViewBuilder<_>(id)

[<AutoOpen>]
module HtmlElementsApi =
    let app = Gen.read<App>

    let inline syncChildren (elem: Panel) (children: FrameworkElement list) =
        // TODO: Performance
        do elem.Children.Clear()
        do for child in children do
            printfn $"Sync child: {child.GetType().Name}"
            elem.Children.Add child |> ignore
    
    let inline panel factory =
        ViewBuilder <| fun children ->
            gen {
                let! elem = Gen.preserve factory
                let! children = children
                do syncChildren elem children
                return elem
            }
    
    let span content =
        gen {
            let! elem = Gen.preserve TextBlock
            elem.Text <- content
            return elem
        }

    let inline div (attributes: _ list) =
        panel (fun () -> StackPanel(Orientation = Orientation.Vertical))

    let inline p attributes =
        panel (fun () -> StackPanel(Orientation = Orientation.Horizontal))

    let inline button attributes click =
        ViewBuilder <| fun children ->
            gen {
                let! app = app
                let! sp = Gen.preserve (fun () -> StackPanel(Orientation = Orientation.Horizontal))
                let! button = Gen.preserve (fun () ->
                    let button = Button()
                    button.Content <- sp
                    button
                )
                
                let! subscription,setSubscription = Gen.ofMutable<IDisposable option, _> None
                do match subscription with
                    | Some subscription -> subscription.Dispose()
                    | _ -> ()
                button.Click.Subscribe(fun _ ->
                    printfn "-----CLICK"
                    click ()
                    app.TriggerUpdate())
                |> Some
                |> setSubscription 

                let! children = children
                do syncChildren sp children
                return button
            }

let textInst() = span "test"
// TODO: Value restriction
// let divInst = div [] { () }
// let buttonInst = button [] id { () }



let spanInst() = span "test"
// TODO: Value restriction
let inline divInst()  = div [] { () }
let inline divInst2() = div [] { span "xxxx" }
let inline buttonInst() = button [] id { () }

module Tests =
    let test1() =
        pov {
            span "test"
        }

    let test2() =
        pov {
            span "test 1"
            span "test 2"
        }

    let test3() =
        pov {
            span "test 1"
            div [] {
                ()
            }
            span "test 2"
        }

    let test4() =
        pov {
            span "test 1"
            div [] {
                span "inner 1"
                span "inner 2"
            }
            span "test 2"
        }

    let test5() =
        pov {
            let! c1, setCount = Gen.ofMutable 0
            span $"c1 = {c1}"

            div [] {
                span "inner 1"
                span "inner 2"
            }
            span "test 2"
            div [] {()}
        }

    let test6() =
        pov {
            let! c1,_ = Gen.ofMutable 0
            span $"c1 = {c1}"
            
            let! c2,_ = Gen.ofMutable 0
            div [] {
                span $"c2 = {c2}"
                
                let! c3,_ = Gen.ofMutable 0
                span $"c3 = {c3}"
            }
        }

    let test7() =
        pov {
            // TODO: document that this is not working (yield) and not useful.
            // - Maybe Gen.iter?
            // - or `wrap` to emit the spanElement afterwards?
            // - make also a "preserve" example
            let! spanElememt = span "test 1"
            printfn $"Span inner text: {spanElememt.Text}"

            // yield spanElememt
            span "test 2"
        }

module ViewTest1 =

    // TODO: Example: Parametrized components
    let comp =
        pov {
            let! count, setCount = Gen.ofMutable 0
            div [] {
                div []  {
                    span $"BEGIN for ..."
                    for x in 0..3 do
                        span $"count = {count}"
                        button [] (fun () -> setCount (count + 1)) { 
                            span "..." 
                        }
                        span $"    (another x = {x})"
                        span $"    (another x = {x})"
                    span $"END for ..."
                }
            }
        }


    let view =
        pov {
            div [] {
                comp
                div [] {
                    span "Hurz"
                    comp
                }
            }
        }

module ChangeTypeDuringRuntime =
    let view =
        pov {
            div [] {
                let! count,setCount = Gen.ofMutable 0
                
                p [] { span $"count = {count}     /     count / 5 = {count % 5}" }
                button [] (fun () -> setCount (count + 1)) { 
                    span "Increment (+)" 
                }

                if count % 5 = 0 && count <> 0 then
                    div [] {
                        span "YES!!!!"
                        span " ... it's a multiple of 5!"
                    }
                else
                    let! start = Gen.preserve (fun () -> DateTime.Now.Ticks)
                    let! btnCount,setBtnCount = Gen.ofMutable 0
                    div [] {
                        button [] (fun () -> setBtnCount (btnCount + 1)) {
                            span $"You clicked me {btnCount} times" 
                        }
                        span $"State started: {start}"
                    }
            }
        }

module ChangeTypeDuringRuntimeSimple =
    let view =
        pov {
            div [] {
                let! count,setCount = Gen.ofMutable 0
                button [] (fun () -> setCount (count + 1)) {()}

                if count % 3 = 0 && count <> 0 then
                    span "YES!!!!"
                else
                    let! start = Gen.preserve (fun () -> DateTime.Now.ToString())
                    div [] {()}
            }
        }


let startWpf (window: Window) =
    App.Run(window, ChangeTypeDuringRuntime.view) |> ignore
