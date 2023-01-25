module HtmlSpecGenerator

open FSharp.Text.TypedTemplateProvider
open MdnScrape

let [<Literal>] HtmlSpecTemplate = """elementName|attributeName|fsharpAttributeName|types|felizAttr|url|desc
{{for attr in attributes}}{{attr.elementName}}|{{attr.attributeName}}|{{attr.fsharpAttributeName}}|{{attr.types}}|{{attr.felizAttr}}|{{attr.url}}|{{attr.desc}}
{{end}}
"""

type Api = Template<HtmlSpecTemplate>

module Corrections =
    let attrNameCorrections =
        [
            "class", "class'"
            "type", "type'"
            "as", "as'"
            "default", "default'"
            "for", "for'"
            "open", "open'"
            "http-equiv", "httpEquiv"
            "moz-opaque", "mozOpaque"
            "accept-charset", "acceptCharset"
        ]

    let attrExcludes =
        [
            "data-*"
        ]
        
    let elemNameCorrections =
        [
            "base", "base'"
        ]

    let additionalElemAttrs =
        let makeAttr (elemName: string) name typ =
            elemName,
            {
                name = name
                isDeprecated = false
                isNonStandard = false
                desc = ""
                typ = typ
                felizAttr = None
                url = ""
            }
        [
            makeAttr "input" "type" (AttrTyp.Dotnet typeof<string>)
        ]
        |> List.groupBy fst
        |> List.map (fun (k,v) -> k, v |> List.map snd)
        |> Map.ofList

    let elemExcludes =
        [
            "base"
            "data"
            "time"
            "picture"
            "meter"
            "output"
            "details"
            "dialog"
            "slot"
            "template"
            "portal"
        ]

    let globalEvents =
        [
            "onabort"
            //"onautocomplete"
            //"onautocompleteerror"
            "onblur"
            //"oncancel"
            "oncanplay"
            "oncanplaythrough"
            "onchange"
            "onclick"
            //"onclose"
            "oncontextmenu"
            "oncuechange"
            "ondblclick"
            "ondrag"
            "ondragend"
            "ondragenter"
            "ondragleave"
            "ondragover"
            "ondragstart"
            "ondrop"
            "ondurationchange"
            "onemptied"
            "onended"
            "onerror"
            "onfocus"
            "oninput"
            //"oninvalid"
            "onkeydown"
            "onkeypress"
            "onkeyup"
            "onload"
            "onloadeddata"
            "onloadedmetadata"
            "onloadstart"
            "onmousedown"
            "onmouseenter"
            "onmouseleave"
            "onmousemove"
            "onmouseout"
            "onmouseover"
            "onmouseup"
            "onmousewheel"
            "onpause"
            "onplay"
            "onplaying"
            "onprogress"
            "onratechange"
            "onreset"
            //"onresize"
            "onscroll"
            "onseeked"
            "onseeking"
            "onselect"
            //"onshow"
            //"onsort"
            "onstalled"
            "onsubmit"
            "onsuspend"
            "ontimeupdate"
            //"ontoggle"
            "onvolumechange"
            "onwaiting"
        ]

let htmlGlobalAttrsElementBuilderName = "HTMLGlobalAttrsElementBuilder"

let generate (elements: Element list) (globalAttrs: Attr list) = 
    let correctWith altNames name =
        altNames 
        |> List.tryFind (fun x -> fst x = name)
        |> Option.map snd
        |> Option.defaultValue name

    let makeAttr (attr: Attr) elementName =
        let fsharpAttrName = attr.name |> correctWith Corrections.attrNameCorrections
        let typ,toString =
            match attr.typ with
            | AttrTyp.Dotnet typ ->
                let toString =
                    match typ with
                    | t when t = typeof<string> -> ""
                    | _ -> ".ToString()"
                typ.FullName, toString
            | AttrTyp.Enum labels -> "string", ""
            | AttrTyp.Choice labels -> "string", ""
        let felizAttr = 
            attr.felizAttr 
            |> Option.map (fun f -> 
                let types = f.typs |> List.map (fun t -> t.Name)
                {| name = f.name; types = types |})
            |> Serialize.serializeNoIndent
        let desc = attr.desc.Replace("\n", "--").Replace("|", ";")[0..50]
        Api.attr(attr.name, desc, elementName, felizAttr, fsharpAttrName, typ, attr.url)

    let makeAttrs (attrs: Attr list) elementName =
        attrs
        |> List.distinctBy (fun a -> a.name)
        |> List.sortBy (fun x -> x.name)
        |> List.filter (fun attr -> Corrections.attrExcludes |> List.contains attr.name |> not)
        |> List.map (fun a -> makeAttr a elementName)

    ////let globalElem =
    ////    let ctorParams = "tagName"
    ////    Api.elem(
    ////        globalAttrs |> makeAttrs,
    ////        htmlGlobalAttrsElementBuilderName,
    ////        ctorParams,
    ////        ctorParams,
    ////        Corrections.globalEvents |> List.map Api.evt,
    ////        htmlGlobalAttrsElementBuilderName,
    ////        true,
    ////        "'n",
    ////        "HTMLElementBuilder",
    ////        false,
    ////        "<'n when 'n :> HTMLElement>"
    ////    )

    ////let makeElem (elem: Element) =
    ////    let events = [] //Corrections.globalEvents |> List.map Api.evt
    ////    let attrs =
    ////        (
    ////            elem.attrs.attrs
    ////            @ (Corrections.additionalElemAttrs |> Map.tryFind elem.name |> Option.defaultValue [])
    ////        )
    ////        |> makeAttrs
    ////    let fsharpTageName = elem.name |> correctWith Corrections.elemNameCorrections
    ////    let inheritorName = 
    ////        if elem.attrs.includeGlobalAttrs 
    ////        then htmlGlobalAttrsElementBuilderName
    ////        else "HTMLElementBuilder"
    ////    let tagName = $""" "{elem.name}" """
    ////    Api.elem(
    ////        attrs,
    ////        fsharpTageName,
    ////        tagName,
    ////        "",
    ////        events, 
    ////        fsharpTageName,
    ////        false,
    ////        elem.domInterfaceName,
    ////        inheritorName,
    ////        true,
    ////        ""
    ////    )

    ////let elems =
    ////    globalElem
    ////    :: (
    ////        elements
    ////        |> List.filter (fun e -> Corrections.elemExcludes |> List.contains e.name |> not)
    ////        |> List.map makeElem
    ////    )
    ////    |> List.sortBy (fun x -> x.fsharpTageName)


    let attrs =
        [ for elem in elements do
            yield! makeAttrs elem.attrs.attrs elem.name
        ]
    
    let root = Api.Root(attrs)

    Api.Render(root)
