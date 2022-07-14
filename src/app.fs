module App

open LocSta
open Browser
open Browser.Types

type Sender = Id of int

// TODO: Use reader instead of this hack
[<AllowNullLiteral>]
type App(appElement: HTMLElement, triggerUpdate: Sender option -> HTMLElement option) =
    let mutable currId = -1
    member val CurrentSender: Sender option = None with get, set
    member _.NewSender() =
        currId <- currId + 1
        printfn $"New sender: {currId}"
        Id currId
    member _.CreateElement name =
        printfn $"Create: {name}"
        document.createElement name
    member _.Run() =
        let initialElement = triggerUpdate None
        match initialElement with
        | Some element -> appElement.appendChild element |> ignore
        | None -> printfn "NO INITIAL ELEMENT GIVEN"
    member this.TriggerUpdate sender =
        this.CurrentSender <- sender
        printfn $"Trigger update with sender: {sender}"
        let element = triggerUpdate sender
        match element with
        | None -> printfn "NONE"
        | Some element -> ()
        // TODO: element <> appElement.child -> throw
        ()

let mutable app: App = null

let toSeq (coll: NodeList) = seq { for i in 0..coll.length-1 do coll.Item i }

let elem name attributes child =
    feed {
        let! elem = InitWith (fun () -> app.CreateElement name)
        printfn $"Eval: {name} ({elem.GetHashCode()})"
        let! child = child |> Gen.map (fun x -> x :> Node)
        do for aname,avalue in attributes do
            let elemAttr = elem.attributes.getNamedItem aname
            if elemAttr.value <> avalue then
                elemAttr.value <- avalue
        if toSeq elem.childNodes |> Seq.contains child |> not then
            printfn $"add child (node count = {elem.childNodes.length})"
            elem.appendChild child |> ignore
        return Feed.Emit (elem, elem)
    }

let text content = feed {
    let! elem = InitWith (fun () -> document.createTextNode content)
    do if elem.textContent <> content then
        elem.textContent <- content
    return Feed.Emit (elem, elem)
}
let div attributes content = elem "div" attributes content
let p attributes content = elem "p" attributes content
let button content click = loop {
    let! clickId = Gen.initWith app.NewSender
    if app.CurrentSender = Some clickId then
        app.CurrentSender <- None
        click()
        printfn "SKIP"
        // This means: Skip and implicitly reevaluate
        // TODO: Skip in Reevaluate umbenennen?
        // TODO: Wie funktioniert Skip nochmal genau? :)
        return Loop.Skip
    else
        let! button = elem "button" [] content |> Gen.map (fun x -> x :?> HTMLButtonElement)
        button.onclick <- fun _ ->
            printfn "-----CLICK"
            app.TriggerUpdate (Some clickId)
        button
}

let view() = loop {
    let comp() = loop {
        let! count, setCount = Gen.ofMutable 0
        return!
            div [] (
                button (text $"Count = {count}") (fun () -> setCount (count + 1))
            )
    }

    let! c1 = comp()
    let! c2 = comp()
    let! wrapper = div [] (Gen.initWith (fun () -> document.createTextNode "---"))
    do if wrapper.childNodes.length = 1 then
        wrapper.appendChild c1 |> ignore
        wrapper.appendChild c2 |> ignore
    wrapper
}


do
    let evaluableView = view() |> Gen.toEvaluable
    app <- App(
        document.querySelector("#app") :?> HTMLDivElement,
        fun _ -> evaluableView.Evaluate()
    )

    app.Run()
