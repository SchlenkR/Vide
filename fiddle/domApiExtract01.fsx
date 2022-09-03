
#r "nuget: FSharp.Data"

open FSharp.Data

type HtmlNode with
    member this.CssTryOne(selector) =
        match this.CssSelect(selector) with
        | [node] -> Some node
        | _ -> None
    member this.CssOne(selector) =
        this.CssTryOne(selector).Value
    member this.HasOne(selector) =
        match this.CssTryOne(selector) with
        | Some _ -> Some this
        | _ -> None

//let url = "https://html.spec.whatwg.org"
//let results = HtmlDocument.Load(url)
//let interfaces =
//    [
//        for idef in results.CssSelect("dd[w-nodev] pre code.idl") do
//            let ifName = idef.CssOne("dfn[interface]")
//            ifName.InnerText()
//    ]

HtmlDocument.Load("https://dom.spec.whatwg.org")


let allInterfaces =
    HtmlDocument
        .Load("https://html.spec.whatwg.org/multipage/indices.html#all-interfaces")
        .CssSelect("ul.brief code")
    |> List.choose (fun node -> node.TryGetAttribute("id") |> Option.map (fun idAttr -> node,idAttr.Value()))
    |> List.filter (fun (_,idAttr) -> idAttr.StartsWith("all-interfaces:"))
    |> List.map (fun (node,_) ->
        let a = node.CssOne("a")
        {| linkToDef = a.Attribute("href").Value(); ifName = a.InnerText() |}
    )
