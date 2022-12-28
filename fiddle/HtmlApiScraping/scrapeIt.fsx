
#r "nuget: FSharp.Data"
#r "nuget: FSharp.SystemTextJson"

open System
open FSharp.Data

module List =
    let partitionMap (mapping: 'a -> Choice<'b,'c>) (source: list<'a>) =
        let rec loop ((acc1, acc2) as acc) =
            function
            | [] -> acc
            | x::xs ->
                match mapping x with
                | Choice1Of2 x -> loop (x::acc1, acc2) xs
                | Choice2Of2 x -> loop (acc1, x::acc2) xs
        loop ([], []) (List.rev source)

module String =
    let contains (s: string) (x: string) =
        s.Contains(x, StringComparison.OrdinalIgnoreCase)
    let joinParts parts =
        parts
        |> String.concat ""
        |> fun x -> x.Trim()

module Serialize =
    open System.Text.Json
    open System.Text.Json.Serialization
    
    let options = JsonSerializerOptions()
    options.Converters.Add(JsonFSharpConverter())

    let serialize (o: obj) =
        JsonSerializer.Serialize(o, options)

module Html =
    let loadBody (url: string) =
        printfn $"LOADING URL: {url}"
        HtmlDocument.Load(url).Body()
    let getLabelledSections (x: HtmlNode) =
        x.CssSelect("section")
        |> List.choose (fun x ->
            x.TryGetAttribute("aria-labelledby")
            |> Option.map (fun a -> x, a.Value()))
    let getLabelledSection name (x: HtmlNode) =
        getLabelledSections x
        |> List.find (fun s -> snd s = name)
    let rec extractDescription (nodes: HtmlNode list) : string =
        let descendants = nodes |> List.collect (fun n -> n.Descendants() |> Seq.toList)
        [ for x in descendants do
            match x.Name() with
            | "" | "a" | "strong" | "em" | "code" -> " " + x.InnerText()
            | "p" -> x.Elements() |> extractDescription
            | "ul" | "li" | "div" -> " [ TODO ]"
            | _ as name -> failwith $"Unexpected inner node while extracting description: {name}  -  {x}"
        ]
        |> String.joinParts
        |> fun x -> x.Replace("  ", " ")
    let buildDtAndDd (dl: HtmlNode) =
        dl
        |> fun x -> x.Elements()
        |> List.pairwise
        |> List.mapi (fun i x -> i,x)
        |> List.filter (fun (i,x) -> i % 2 = 0)
        |> List.map snd
        
let failWithCtx(ctx, f) =
    try f() with ex -> failwith $"%s{ctx} :: {ex.Message}"

let mdnRoot = "https://developer.mozilla.org"


//let attrs =
//    let attributesPage = Html.loadBody(mdnRoot + "/en-US/docs/Web/HTML/Attributes")
//    let attrRows =
//        Html.getLabelledSections attributesPage
//        |> List.map fst
//        |> fun s -> s.CssSelect(".section-content").CssSelect("table tbody tr")
//    [ for tr in attrRows do
//        let tds = tr.CssSelect("td")
//        let attrName, attrLink = 
//            match tds[0].CssSelect("a") with
//            | [a] -> a.InnerText(), Some (mdnRoot + a.Attribute("href").Value())
//            | [] -> tds[0].CssSelect("code").Head.InnerText(), None
//            | _ -> failwith $"Not found: {tds[0]}"
//            |> fun (name,link) -> name,link
//        let attrDesc,attrNote =
//            let getNoteSection (x: HtmlNode) =
//                [ for y in x.CssSelect("p").Head.Elements() do
//                    match y.Name() with
//                    | "" | "a" | "strong" | "p" -> y.InnerText()
//                    | _ as name -> failwith $"Unexpected note-desc inner node: {name}  -  {x} / {y}"
//                ]
//                |> String.concat ""
//            [ for x in tds[2].Elements() do
//                match x.Name() with
//                | "" | "a" | "code" | "p" -> 
//                    x.InnerText() |> Choice1Of2
//                | "div" -> // propably a 'Note' section
//                    getNoteSection x
//                    |> Choice2Of2
//                | _ as name -> failwith $"Unexpected desc inner node: {name}  -  {x}"
//            ]
//            |> List.partitionMap id
//            |> fun (descParts,noteParts) ->
//                let desc = String.joinParts descParts
//                let note =
//                        match String.joinParts noteParts with 
//                        | "" | null -> None
//                        | _ as x -> Some x
//                desc,note
//        let isObsolete = 
//            attrNote
//            |> Option.map (fun note -> note |> String.contains("obsolete") || note |> String.contains("legacy"))
//            |> Option.defaultValue false
//        let applicableElements =
//            tds[1].CssSelect("code")
//            |> List.map (fun x -> 
//                match x.InnerText() with
//                | "Global attribute" as x -> x
//                | s -> s[1 .. ^1] // <yyy> -> yyy
//            )
//        {|
//            attribute =
//                {|
//                    name = attrName
//                    link = attrLink
//                    desc = attrDesc
//                    note = attrNote
//                    isObsolete = isObsolete
//                |}
//            applicableElements = applicableElements
//        |}
//    ]

type Label = { name: string; desc: string }

[<RequireQualifiedAccess>]
type AttrTyp =
    | Text
    | Color
    | Integer
    | Float
    | Enum of Label list
    | Choice of Label list

let elements =
    let elementsPage = Html.loadBody(mdnRoot + "/en-US/docs/Web/HTML/Element")

    let getAttrs url =
        let url = mdnRoot + url
        let elementPage = Html.loadBody(url)
        let attrsSection = 
            elementPage 
            |> Html.getLabelledSections
            |> List.tryFind (fun x -> snd x = "attributes")
        let includeGlobalAttrs =
            match attrsSection with
            | None -> false
            | Some attrsSection ->
                (fst attrsSection).CssSelect("a")
                |> List.exists (fun a -> a.InnerText() |> String.contains "global attributes")
        let attrs =
            match attrsSection with
            | None -> []
            | Some (attrsSection,_) ->
                let dtAndDd =
                    let dl =
                        attrsSection.CssSelect(".section-content")
                        |> List.exactlyOne
                        |> fun x -> x.Elements("dl")
                        |> List.tryHead
                    match dl with
                    | Some dl -> Html.buildDtAndDd dl
                    | None -> []
                [ for dt,dd in dtAndDd do
                    let name = failWithCtx($"dt.css(code) {dt}", fun () -> 
                        dt.CssSelect("code") |> List.exactlyOne |> fun x -> x.InnerText)
                    let isDeprecated,isNonStandard =
                        let abbrs = dt.CssSelect("abbr")
                        let has x = abbrs |> List.exists (fun abbr -> abbr.HasClass(x))
                        has "icon-deprecated", has "icon-nonstandard"
                    let desc = dd.CssSelect("p") |> Html.extractDescription
                    let typ =
                        // TODO: far from complete
                        match dd.Elements("dl") with
                        | [] ->
                            match dd.Elements("ul") with
                            | [] ->
                                if desc |> String.contains "hexade" then AttrTyp.Color
                                elif desc |> String.contains "integer" then AttrTyp.Integer
                                else AttrTyp.Text
                            | [ul] ->
                                let lis = 
                                    ul.Elements("li")
                                    |> List.choose (fun li ->
                                        li.Elements("code")
                                        |> List.tryHead
                                    )
                                [ for li in lis do
                                    let label = li.InnerText()
                                    let desc = li.Elements() |> Html.extractDescription
                                    { name = label; desc = desc }
                                ]
                                |> AttrTyp.Enum
                                
                            | _ -> failwith $"could not infer type of attr (nore than 1 ull found) - {dd}"
                        | [dl] -> AttrTyp.Choice []
                        | _ -> failwith $"could not infer type of attr (nore than 1 dl found) - {dd}"

                    {|
                        name = name
                        isDeprecated = isDeprecated
                        isNonStandard = isNonStandard
                        desc = desc
                        typ = typ
                    |}
                ]
        {|
            includeGlobalAttrs = includeGlobalAttrs
            attrs = attrs
        |}

    let sections = Html.getLabelledSections elementsPage
    [ for sec, secCategory in sections do
        let trs = sec.CssSelect(".standard-table tbody tr")
        [ for tr in trs do
            let tds = tr.CssSelect("td")

            let elements =
                tds[0].CssSelect("a")
                |> List.map (fun a ->
                    let link = a.Attribute("href").Value()
                    let attrs = getAttrs link
                    {|
                        category = secCategory
                        name = 
                            a.Elements() 
                            |> List.exactlyOne 
                            |> fun x -> x.InnerText()
                            |> fun x -> x[1 .. ^1]
                        desc = tds[1].Elements() |> Html.extractDescription
                        link = link
                        attrs = attrs
                    |}
                )
            elements
        ]
        |> List.collect id
    ]
    |> List.collect id





//Serialize.serialize attrs
//|> fun x -> File.WriteAllText("c:\\temp\\attrs.json", x)
