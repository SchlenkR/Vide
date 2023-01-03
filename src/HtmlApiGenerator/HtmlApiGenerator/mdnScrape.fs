
#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: FSharp.SystemTextJson"
#r "nuget: FsHttp"
#r "nuget: Feliz"
#else
module MdnScrape
#endif

open System
open System.IO
open System.Reflection
open FSharp.Data
open FsHttp

let mdnRoot = "https://developer.mozilla.org"
let cacheDir = Path.Combine(__SOURCE_DIRECTORY__, "cache")

type Label = 
    { 
        name: string
        desc: string 
    }

[<RequireQualifiedAccess>]
type AttrTyp =
    | Dotnet of Type
    | Enum of Label list
    | Choice of Label list

type FelizAttr =
    {
        name: string
        altNames: Set<string>
        typs: Type list
    }

type Attr =
    {
        name: string
        isDeprecated: bool
        isNonStandard: bool
        desc: string
        typ: AttrTyp
        felizAttr: FelizAttr option
    }

type Attrs =
    {
        includeGlobalAttrs: bool
        attrs: Attr list
    }

type Element =
    {
        category: string
        name: string
        domInterfaceName: string
        desc: string
        link: string
        attrs: Attrs
    }

[<AutoOpen>]
module Helper =
    let urlCombine (url1: string) (url2: string) =
        let del = '/'
        let sdel = string del
        let norm (s: string) = s.Trim().Replace(@"\", sdel)
        let delTrim = [| del |]
        let a = (norm url1).TrimEnd(delTrim)
        let b = (norm url2).TrimStart(delTrim).TrimEnd(delTrim)
        a + sdel + b
    let (</>) = urlCombine

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
        let containsIgnoreCase (s: string) (x: string) =
            s.Contains(x, StringComparison.OrdinalIgnoreCase)

    module Serialize =
        open System.Text.Json
        open System.Text.Json.Serialization
    
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())

        let serialize (o: obj) =
            JsonSerializer.Serialize(o, options)

    module Html =
        let loadBody (relUrl: string) =
            let cachePath = cacheDir </> relUrl + ".html"
            if not (File.Exists(cachePath)) then 
                get (mdnRoot + relUrl)
                |> Request.send
                |> Response.saveFile cachePath
            printfn $"LOADING URL: {relUrl}"
            use fs = File.OpenRead(cachePath)
            HtmlDocument.Load(fs).Body()
        let getLabelledSections (x: HtmlNode) =
            x.CssSelect("section")
            |> List.choose (fun x ->
                x.TryGetAttribute("aria-labelledby")
                |> Option.map (fun a -> x, a.Value()))
        let extractDescription (nodes: HtmlNode list) : string =
            let descendants = nodes |> List.collect (fun n -> n.DescendantsAndSelf() |> Seq.toList)
            [ for x in descendants do 
                x.InnerText()
            ]
            |> String.concat ""
            |> fun x -> x.Trim()
            |> fun x -> x.Replace("  ", " ")
        let buildDtAndDd (dl: HtmlNode) =
            dl
            |> fun x -> x.Elements()
            |> List.pairwise
            |> List.mapi (fun i x -> i,x)
            |> List.filter (fun (i,x) -> i % 2 = 0)
            |> List.map snd

    type Type with
        member this.Show() =
            let genArgs = this.GetGenericArguments()
            match genArgs with
            | [||] -> this.Name
            | args -> 
                let n = this.Name.Substring(0, this.Name.IndexOf("`"))
                let genArgsNames = args |> Seq.map (fun x -> x.Show()) |> String.concat ", "
                $"{n}<{genArgsNames}>"

    module Types =
        let show (this: Type list) =
            this |> List.map (fun t -> t.Show()) |> String.concat "\n"

    module AttrTyp =
        let show this =
            let labelsToString (labels: Label list) =
                labels |> List.map (fun x -> x.name) |> String.concat ", "
            match this with
            | AttrTyp.Dotnet typ -> typ.Show()
            | AttrTyp.Enum labels -> $"Enum ({labelsToString labels})"
            | AttrTyp.Choice labels -> $"Choice ({labelsToString labels})"
        
    let failWithCtx(ctx, f) =
        try f() with ex -> failwith $"%s{ctx} :: {ex.Message}"

        

let generate () =

    let felizAttrs =
        let propType = typeof<Feliz.prop>
        let attrRetType = typeof<Feliz.IReactProperty>
        let staticAttrsAsMethod = 
            propType.GetMethods(BindingFlags.Static ||| BindingFlags.Public)
            |> Seq.filter (fun m -> m.ReturnType = attrRetType)
            |> Seq.choose (fun m -> 
                match m.GetParameters() with
                | [| p |] -> Some (m.Name, p)
                | _ ->
                    printfn $"WARNING - Unexpected Feliz attr overload detected: {m}"
                    None
            )
            |> Seq.groupBy fst
            |> Seq.map (fun (name, overloads) ->
                let typs = overloads |> Seq.map snd |> Seq.map (fun x -> x.ParameterType) |> Seq.toList
                { 
                    name = name
                    altNames = 
                        [
                            name
                            name.ToLowerInvariant()
                            name.Replace("-", "")
                            name.ToLowerInvariant().Replace("-", "")
                        ]
                        |> Set.ofList
                    typs = typs 
                }
            )
            |> Seq.sortBy (fun x -> x.name)
            |> Seq.toList
        staticAttrsAsMethod

    let buildAttrs labelledSectionId relUrl includeDomIfExtraction =
        let elementPage = Html.loadBody(relUrl)
        let attrsSection = 
            elementPage 
            |> Html.getLabelledSections
            |> List.tryFind (fun x -> snd x = labelledSectionId)
        let includeGlobalAttrs =
            match attrsSection with
            | None -> false
            | Some attrsSection ->
                (fst attrsSection).CssSelect("a")
                |> List.exists (fun a -> a.InnerText() |> String.containsIgnoreCase "global attributes")
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
                    let name = 
                        dt.CssSelect("code") 
                        |> List.exactlyOne |> fun x -> x.InnerText()
                    let isDeprecated,isNonStandard =
                        let abbrs = dt.CssSelect("abbr")
                        let has x = abbrs |> List.exists (fun abbr -> abbr.HasClass(x))
                        has "icon-deprecated", has "icon-nonstandard"
                    let desc = dd.Elements() |> Html.extractDescription
                    let typ =
                        // TODO: far from complete
                        match dd.Elements("dl") with
                        | [] ->
                            match dd.Elements("ul") with
                            | [] ->
                                if desc |> String.containsIgnoreCase "hexade" then AttrTyp.Dotnet (typeof<string>)
                                elif desc |> String.containsIgnoreCase "integer" then AttrTyp.Dotnet (typeof<int>)
                                else AttrTyp.Dotnet (typeof<string>)
                            | [ul] ->
                                let lis = 
                                    ul.Elements("li")
                                    |> List.choose (fun li ->
                                        li.Elements("code")
                                        |> List.tryHead
                                    )
                                let fields =
                                    [ for li in lis do
                                        let label = li.InnerText()
                                        let desc = li.Elements() |> Html.extractDescription
                                        { name = label; desc = desc }
                                    ]
                                let isBBool =
                                    let b1 = ["true"; "false"] |> Set.ofList
                                    let b2 = fields |> List.map (fun x -> x.name) |> Set.ofList
                                    b1 = b2
                                if isBBool then
                                    AttrTyp.Dotnet (typeof<bool>)
                                else
                                    let x = if desc |> String.containsIgnoreCase "enum" then AttrTyp.Enum else AttrTyp.Choice
                                    x fields
                            
                            | _ -> failwith $"could not infer type of attr (nore than 1 ull found) - {dd}"
                        | [dl] -> AttrTyp.Choice []
                        | _ -> failwith $"could not infer type of attr (nore than 1 dl found) - {dd}"

                    {
                        name = name
                        isDeprecated = isDeprecated
                        isNonStandard = isNonStandard
                        desc = desc
                        typ = typ
                        felizAttr = felizAttrs |> List.tryFind (fun a -> a.altNames |> Set.contains name)
                    }
                ]
        let domInterfaceName =
            if includeDomIfExtraction then
                elementPage.CssSelect("tr")
                |> List.find (fun tr -> tr.Elements().Head.InnerText().Equals("DOM interface", StringComparison.OrdinalIgnoreCase))
                |> fun tr -> (tr.Elements()[1]).CssSelect("a").Head.CssSelect("code").Head.InnerText()
            else
                ""
        let res =
            {
                includeGlobalAttrs = includeGlobalAttrs
                attrs = attrs
            }
        res,domInterfaceName

    let globalAttrs = 
        buildAttrs "list_of_global_attributes" "/en-US/docs/Web/HTML/Global_attributes#list_of_global_attributes" false
        |> fun x -> (fst x).attrs

    let elements =
        let elementsPage = Html.loadBody("/en-US/docs/Web/HTML/Element")
        let sections = 
            Html.getLabelledSections elementsPage
            |> List.filter (fun x -> snd x <> "obsolete_and_deprecated_elements")
        [ for sec, secCategory in sections do
            let trs = sec.CssSelect(".standard-table tbody tr")
            [ for tr in trs do
                let tds = tr.CssSelect("td")
                let elements =
                    tds[0].CssSelect("a")
                    |> List.map (fun a ->
                        // this is actually "the" element
                        let link = a.Attribute("href").Value()
                        let attrs = buildAttrs "attributes" link true
                        {
                            category = secCategory
                            name = 
                                a.Elements() 
                                |> List.exactlyOne 
                                |> fun x -> x.InnerText()
                                |> fun x -> x[1 .. ^1]
                            domInterfaceName = snd attrs
                            desc = 
                                tds[1].Elements()
                                |> Html.extractDescription
                            link = link
                            attrs =
                                let attrs = fst attrs
                                if attrs.includeGlobalAttrs
                                    then { attrs with attrs = globalAttrs @ attrs.attrs }
                                    else attrs
                        }
                    )
                elements
            ]
            |> List.collect id
        ]
        |> List.collect id

    {| felizAttrs = felizAttrs; globalAttrs = globalAttrs; elements = elements |}




















module MdTest =

    [<AutoOpen>]
    module Md =
        let norm (s: string) =
            s
                .Replace("|", " - ")
                .Replace("\r", "")
                .Replace("\n", " -- ")
        let htmlToMd (s: string) =
            (norm s)
                .Replace("\n", "<br />")
                .Replace("<", "(")
                .Replace(">", ")")
        let nl = "\n"
        let br = ""
        let h1 s = (norm s) + "\n===\n"
        let h2 s = (norm s) + "\n---"
        let kvp key value = $"<strong>{key}:</strong> {htmlToMd (value.ToString())}\n"
        let quote (s: string) =
            s.Split(nl)
            |> Seq.map (fun x -> $"> {norm x}")
            |> String.concat nl
        let table (colHeadings: string list) (rows: string list list) =
            let mkRow x =
                [ 
                    for x in x do
                        $"| {norm x} "
                    " |"
                ]
                |> String.concat ""
            [
                mkRow colHeadings
                mkRow (colHeadings |> List.map (fun _ -> "---"))
                for row in rows do
                    mkRow row
                nl
            ]
            |> String.concat nl

    let result = generate ()

    let mdOutputFolder = "c:/temp/videOutput/md"

    let mdElements =
        let renderAttrs attrs =
            table
                [
                    "name"
                    "desc"
                    "isDeprecated"
                    "isNonStandard"
                    "typ"
                    "feliz attr"
                ]
                (
                    attrs
                    |> List.filter (fun a -> not a.isDeprecated)
                    |> List.map (fun a -> [
                        a.name
                        htmlToMd a.desc
                        a.isDeprecated.ToString()
                        a.isNonStandard.ToString()
                        AttrTyp.show a.typ
                        (
                            a.felizAttr
                            |> Option.map (fun x -> x.typs |> Types.show)
                            |> Option.defaultValue "-"
                        )
                    ])
                )

        [
            h1 "Global Attributes"
            renderAttrs result.globalAttrs
            br

            for e in result.elements do
                h1 e.name
                kvp "Category" e.category
                kvp "MDN" (mdnRoot + e.link)
                kvp "DOM Interface" e.domInterfaceName
                br
            
                htmlToMd e.desc
                br

                h2 "Attributes"
                kvp "Global attributes" e.attrs.includeGlobalAttrs
                if e.attrs.attrs.Length > 0 then
                    renderAttrs e.attrs.attrs
                br
        ]
        |> String.concat nl

    // File.WriteAllText(mdOutputFolder </> "/elements.md", mdElements)

    let mdFeliz =
        [ 
            h1 "Feliz Attributes"
            table 
                [
                    "name"
                    "typ"
                ]
                [ for a in result.felizAttrs do
                    [
                        a.name
                        Types.show a.typs
                    ]
                ]
        ]
        |> String.concat nl

    // File.WriteAllText(mdOutputFolder </>  "felizAttrs.md", mdFeliz)
