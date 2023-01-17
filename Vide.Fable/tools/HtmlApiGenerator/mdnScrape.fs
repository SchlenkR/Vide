
#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: FSharp.SystemTextJson"
#r "nuget: FsHttp"
#r "nuget: Feliz"
#else
module w3schoolScrape
#endif

open System
open System.IO
open FSharp.Data
open FsHttp

let w3sRoot = "https://www.w3schools.com"
let cacheDir = Path.Combine(__SOURCE_DIRECTORY__, "w3sCache")

type Label = 
    { 
        name: string
        desc: string 
    }

[<RequireQualifiedAccess>]
type AttrTyp =
    | Boolean
    | Text
    | Enum of Label list

type Attr =
    {
        name: string
        desc: string
        types: AttrTyp list
        link: string
    }

type Element =
    {
        name: string
        domInterfaceName: string
        desc: string
        link: string
        includeGlobalAttrs: bool
        includeGlobalEvents: bool
        attrs: Attr list
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

    module Html =
        let loadBody (relUrl: string) =
            let cachePath = cacheDir </> relUrl + ".html"
            if not (File.Exists(cachePath)) then 
                get (w3sRoot </> relUrl)
                |> Request.send
                |> Response.saveFile cachePath
            printfn $"LOADING URL: {relUrl}"
            use fs = File.OpenRead(cachePath)
            HtmlDocument.Load(fs).Body()


let generate () =

    let elements =
        let tryFindTable (node: HtmlNode) skipRows =
            node.CssSelect(".ws-table-all") 
            |> List.tryExactlyOne
            |> Option.map (fun t ->
                t.CssSelect("tr")[skipRows..]
            )
        let findTable (node: HtmlNode) skipRows =
            (tryFindTable node skipRows).Value
        let getHRef (a: HtmlNode) = a.Attribute("href").Value()
        let getNameAndLink (node: HtmlNode) =
            let a = node.CssSelect("a") |> List.exactlyOne
            a.InnerText(), getHRef a

        let elementsPage = Html.loadBody("/tags/default.asp")
        let elementRows = findTable elementsPage 2
        [ for elemTr in elementRows do
            let elemTds = elemTr.CssSelect("td")
            let elemName,elemLink = getNameAndLink (elemTds[0])
            let elemDesc = elemTds[1].InnerText()
            let includeGlobalAttrs =
                elementsPage.CssSelect("a")
                |> List.exists (fun a -> getHRef a = "ref_standardattributes.asp")
            let includeGlobalEvents =
                elementsPage.CssSelect("a")
                |> List.exists (fun a -> getHRef a = "ref_eventattributes.asp")
            let attrs =
                let elementPage = Html.loadBody("/tags" </> elemLink)
                let attrRows = tryFindTable elementPage 1 |> Option.defaultValue []
                [ for attrTr in attrRows do
                    let attrTds = attrTr.CssSelect("td")
                    let attrName,attrLink = getNameAndLink (attrTds[0])
                    let attrDesc = attrTds[2].InnerText()
                    let attrTypes =
                        let attrPage = Html.loadBody("/tags" </> attrLink)
                        let isBoolean = attrPage.InnerText().Contains("attribute is a boolean attribute")
                        let attrType =
                            match tryFindTable attrPage 1 with
                            | None -> AttrTyp.Boolean
                            | Some [_] -> AttrTyp.Text
                            | Some trs ->
                                [ for tr in trs do
                                    let tds = tr.CssSelect("td")
                                    let labelName =
                                        tds[0].CssSelect("a")
                                        |> List.tryExactlyOne
                                        |> Option.map (fun a -> a.InnerText())
                                        |> Option.defaultWith (tds[0].InnerText)
                                    let labelDesc = tds[1].InnerText()
                                    { Label.name = labelName; desc = labelDesc }
                                ]
                                |> AttrTyp.Enum
                        [
                            if isBoolean then AttrTyp.Boolean
                            AttrTyp.Text
                            attrType
                        ]
                        |> List.distinct
                    let finalAttr =
                        {
                            Attr.name = attrName
                            desc = attrDesc
                            types = attrTypes
                            link = attrLink
                        }
                    finalAttr
                ]
                |> List.sortBy (fun e -> e.name)
            {
                Element.name = elemName
                domInterfaceName = "TODO"
                desc = elemDesc
                link = elemLink
                includeGlobalAttrs = includeGlobalAttrs
                includeGlobalEvents = includeGlobalEvents
                attrs = attrs
            }

        ]
        |> List.sortBy (fun e -> e.name)

    {| elements = elements |}


let res = generate()
