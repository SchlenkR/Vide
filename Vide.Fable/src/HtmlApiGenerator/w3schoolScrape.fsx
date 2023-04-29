
#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: FSharp.SystemTextJson"
#r "nuget: FsHttp"
#r "nuget: Feliz"
#else
module W3schoolScrape
#endif

open System
open System.IO
open FSharp.Data
open FsHttp

let w3sRoot = "https://www.w3schools.com"
let cacheDir = Path.Combine(__SOURCE_DIRECTORY__, ".cache/w3sCache")

type Label = 
    { 
        name: string
        fsharpName: string
        desc: string 
    }

type BooleanAttrStyle =
    | Present
    | TrueFalse

[<RequireQualifiedAccess>]
type AttrTyp =
    | Boolean of BooleanAttrStyle
    | Text
    | Enum of Label list

type AttrSetMode =
    | SetAttribute
    | DomPropertySetter

type Attr =
    {
        name: string
        fsharpName: string
        desc: string
        types: AttrTyp list
        setMode: AttrSetMode
        link: string option
    }

type Evt =
    {
        name: string
        desc: string
    }

type VoidOrContent =
    | Void
    | Content

type Element =
    {
        tagName: string
        fsharpName: string
        domInterfaceName: string
        elementType: VoidOrContent
        returnsValue: bool
        desc: string
        link: string
        includeGlobalAttrs: bool
        includeGlobalEvents: bool
        attrs: Attr list
        events: Evt list
    }

// generated from mdnScrape
let domInterfaceMap =
    [
        "a", "HTMLAnchorElement" 
        "abbr", "HTMLElement" 
        "address", "HTMLElement" 
        "area", "HTMLAreaElement" 
        "article", "HTMLElement" 
        "aside", "HTMLElement" 
        "audio", "HTMLAudioElement" 
        "b", "HTMLElement" 
        "base", "HTMLBaseElement" 
        "bdi", "HTMLElement" 
        "bdo", "HTMLElement" 
        "blockquote", "HTMLQuoteElement" 
        "body", "HTMLBodyElement" 
        "br", "HTMLBRElement" 
        "button", "HTMLButtonElement" 
        "canvas", "HTMLCanvasElement" 
        "caption", "HTMLTableCaptionElement" 
        "cite", "HTMLElement" 
        "code", "HTMLElement" 
        "col", "HTMLTableColElement" 
        "colgroup", "HTMLTableColElement" 
        "data", "HTMLElement" // "HTMLDataElement" 
        "datalist", "HTMLDataListElement" 
        "dd", "HTMLElement" 
        "del", "HTMLModElement" 
        "details", "HTMLElement" // "HTMLDetailsElement" 
        "dfn", "HTMLElement" 
        "dialog", "HTMLDialogElement" 
        "div", "HTMLDivElement" 
        "dl", "HTMLDListElement" 
        "dt", "HTMLElement" 
        "em", "HTMLElement" 
        "embed", "HTMLEmbedElement" 
        "fieldset", "HTMLFieldSetElement" 
        "figcaption", "HTMLElement" 
        "figure", "HTMLElement" 
        "footer", "HTMLElement" 
        "form", "HTMLFormElement" 
        "h1", "HTMLHeadingElement" 
        "h2", "HTMLHeadingElement" 
        "h3", "HTMLHeadingElement" 
        "h4", "HTMLHeadingElement" 
        "h5", "HTMLHeadingElement" 
        "h6", "HTMLHeadingElement" 
        "head", "HTMLHeadElement" 
        "header", "HTMLElement" 
        "hr", "HTMLHRElement" 
        "html", "HTMLHtmlElement" 
        "i", "HTMLElement" 
        "iframe", "HTMLIFrameElement" 
        "img", "HTMLImageElement" 
        "input", "HTMLInputElement" 
        "ins", "HTMLModElement" 
        "kbd", "HTMLElement" 
        "label", "HTMLLabelElement" 
        "legend", "HTMLLegendElement" 
        "li", "HTMLLIElement" 
        "link", "HTMLLinkElement" 
        "main", "HTMLElement" 
        "map", "HTMLMapElement" 
        "mark", "HTMLElement" 
        "menu", "HTMLMenuElement" 
        "meta", "HTMLMetaElement" 
        "meter", "HTMLElement" // "HTMLMeterElement" 
        "nav", "HTMLElement" 
        "noscript", "HTMLElement" 
        "object", "HTMLObjectElement" 
        "ol", "HTMLOListElement" 
        "optgroup", "HTMLOptGroupElement" 
        "option", "HTMLOptionElement" 
        "output", "HTMLElement" // "HTMLOutputElement" 
        "p", "HTMLParagraphElement" 
        "param", "HTMLParamElement"
        "picture", "HTMLElement" // "HTMLPictureElement" 
        "portal", "HTMLPortalElement" 
        "pre", "HTMLPreElement" 
        "progress", "HTMLProgressElement" 
        "q", "HTMLQuoteElement" 
        "rp", "HTMLElement" 
        "rt", "HTMLElement" 
        "ruby", "HTMLElement" 
        "s", "HTMLElement" 
        "samp", "HTMLElement" 
        "script", "HTMLScriptElement" 
        "section", "HTMLElement" 
        "select", "HTMLSelectElement" 
        "slot", "HTMLElement"  //"HTMLSlotElement" 
        "small", "HTMLElement" 
        "source", "HTMLSourceElement" 
        "span", "HTMLSpanElement" 
        "strong", "HTMLElement" 
        "style", "HTMLStyleElement" 
        "sub", "HTMLElement" 
        "summary", "HTMLElement" 
        "sup", "HTMLElement" 
        "table", "HTMLTableElement" 
        "tbody", "HTMLTableSectionElement" 
        "td", "HTMLTableCellElement" 
        "template", "HTMLElement" // "HTMLTemplateElement" 
        "textarea", "HTMLTextAreaElement" 
        "tfoot", "HTMLTableSectionElement" 
        "th", "HTMLTableCellElement" 
        "thead", "HTMLTableSectionElement" 
        "time", "HTMLElement" // "HTMLTimeElement" 
        "title", "HTMLTitleElement" 
        "tr", "HTMLTableRowElement" 
        "track", "HTMLTrackElement" 
        "u", "HTMLElement" 
        "ul", "HTMLUListElement" 
        "var", "HTMLElement" 
        "video", "HTMLVideoElement" 
        "wbr", "HTMLElement"  
        // --------
    ] 
    |> Map.ofList

let elementExcludes =
    [
        "!--...--"
        "!DOCTYPE"
        "h1 to h6"
        "svg"
        //
    ]

let domPropertySetter =
    [
        "input", "value"
    ]

let additionalGlobalAttributes =
    [
        {
            name = "slot"
            fsharpName = "slot"
            desc = "The slot global attribute assigns a slot in a shadow DOM shadow tree to an element: An element with a slot attribute is assigned to the slot created by the slot element whose name attribute's value matches that slot attribute's value."
            types = [ AttrTyp.Text ]
            setMode = SetAttribute
            link = None
        }
    ]

let additionalElements =
    [
        // slot
        {
            tagName = "slot"
            fsharpName = "slot"
            elementType = Content
            returnsValue = false
            domInterfaceName = "HTMLElement"
            desc = "The slot element—part of the Web Components technology suite is a placeholder inside a web component that you can fill with your own markup, which lets you create separate DOM trees and present them together."
            link = "tag_slot.asp"
            includeGlobalAttrs = true
            includeGlobalEvents = true
            attrs = [
                {
                    name = "name"
                    fsharpName = "name"
                    desc = "A string used to get and set the slot's name."
                    types = [ AttrTyp.Text ]
                    setMode = SetAttribute
                    link = None
                }
            ]
            events = [
                {
                    name = "slotchange"
                    desc = "The slotchange event is fired on an HTMLSlotElement instance (slot element) when the node(s) contained in that slot change."
                }
            ]
        }

        for x in 1..6 do 
            let tagName = $"h{x}"
            {
                tagName = tagName
                fsharpName = tagName
                elementType = Content
                returnsValue = false
                domInterfaceName = "HTMLHeadingElement"
                desc = "Defines HTML headings"
                link = "tag_hn.asp"
                includeGlobalAttrs = true
                includeGlobalEvents = true
                attrs = []
                events = []
            }
    ]

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
        "checked", "checked'"
    ]

let attrExcludes =
    [
        "data-*"
    ]
    
let elemNameCorrections =
    [
        "base", "base'"
    ]

let voidElements =
    [
        "area"
        "base"
        "br"
        "col"
        "embed"
        "hr"
        "img"
        "input"
        "keygen"
        "link"
        "meta"
        "param"
        "source"
        "track"
        "wbr"
    ]

let elementsWithReturnValue =
    [
        "input"
        //"label"
        "select"
        "textarea"
        //"button"
        //"fieldset"
        //"legend"
        "datalist"
        "output"
        "option"
        //"optgroup"
    ]


let eventDefinitions =
    {|
        // Events triggered for the window object (applies to the <body> tag):
        windowEvents = [
            "onafterprint", "Script to be run after the document is printed"
            "onbeforeprint", "Script to be run before the document is printed"
            "onbeforeunload", "Script to be run when the document is about to be unloaded"
            "onerror", "Script to be run when an error occurs"
            "onhashchange", "Script to be run when there has been changes to the anchor part of the a URL"
            "onload", "Fires after the page is finished loading"
            "onmessage", "Script to be run when the message is triggered"
            "onoffline", "Script to be run when the browser starts to work offline"
            "ononline", "Script to be run when the browser starts to work online"
            "onpagehide", "Script to be run when a user navigates away from a page"
            "onpageshow", "Script to be run when a user navigates to a page"
            "onpopstate", "Script to be run when the window's history changes"
            "onresize", "Fires when the browser window is resized"
            "onstorage", "Script to be run when a Web Storage area is updated"
            "onunload", "Fires once a page has unloaded (or the browser window has been closed)"
        ]
        
        // Events triggered by actions inside a HTML form (applies to almost all HTML elements, but is most used in form elements):
        formEvents = [
            "onblur", "Fires the moment that the element loses focus"
            "onchange", "Fires the moment when the value of the element is changed"
            "oncontextmenu", "Script to be run when a context menu is triggered"
            "onfocus", "Fires the moment when the element gets focus"
            "oninput", "Script to be run when an element gets user input"
            //"oninvalid", "Script to be run when an element is invalid"
            "onreset", "Fires when the Reset button in a form is clicked"
            //"onsearch", "Fires when the user writes something in a search field (for <input='search'>)"
            "onselect", "Fires after some text has been selected in an element"
            "onsubmit", "Fires when a form is submitted"
        ]

        keyboardEvents = [
            "onkeydown", "Fires when a user is pressing a key"
            "onkeypress", "Fires when a user presses a key"
            "onkeyup", "Fires when a user releases a key"
        ]

        mouseEvents = [
            "onclick", "Fires on a mouse click on the element"
            "ondblclick", "Fires on a mouse double-click on the element"
            "onmousedown", "Fires when a mouse button is pressed down on an element"
            "onmousemove", "Fires when the mouse pointer is moving while it is over an element"
            "onmouseout", "Fires when the mouse pointer moves out of an element"
            "onmouseover", "Fires when the mouse pointer moves over an element"
            "onmouseup", "Fires when a mouse button is released over an element"
            "onmousewheel", "Deprecated. Use the onwheel attribute instead"
            "onwheel", "Fires when the mouse wheel rolls up or down over an element"
        ]

        dragEvents = [
            "ondrag", "Script to be run when an element is dragged"
            "ondragend", "Script to be run at the end of a drag operation"
            "ondragenter", "Script to be run when an element has been dragged to a valid drop target"
            "ondragleave", "Script to be run when an element leaves a valid drop target"
            "ondragover", "Script to be run when an element is being dragged over a valid drop target"
            "ondragstart", "Script to be run at the start of a drag operation"
            "ondrop", "Script to be run when dragged element is being dropped"
            "onscroll", "Script to be run when an element's scrollbar is being scrolled"
        ]

        clipboardEvents = [
            "oncopy", "Fires when the user copies the content of an element"
            "oncut", "Fires when the user cuts the content of an element"
            "onpaste", "Fires when the user pastes some content in an element"
        ]

        mediaEvents = [
            "onabort", "Script to be run on abort"
            "oncanplay", "Script to be run when a file is ready to start playing (when it has buffered enough to begin)"
            "oncanplaythrough", "Script to be run when a file can be played all the way to the end without pausing for buffering"
            "oncuechange", "Script to be run when the cue changes in a <track> element"
            "ondurationchange", "Script to be run when the length of the media changes"
            "onemptied", "Script to be run when something bad happens and the file is suddenly unavailable (like unexpectedly disconnects)"
            "onended", "Script to be run when the media has reach the end (a useful event for messages like thanks for listening)"
            "onerror", "Script to be run when an error occurs when the file is being loaded"
            "onloadeddata", "Script to be run when media data is loaded"
            "onloadedmetadata", "Script to be run when meta data (like dimensions and duration) are loaded"
            "onloadstart", "Script to be run just as the file begins to load before anything is actually loaded"
            "onpause", "Script to be run when the media is paused either by the user or programmatically"
            "onplay", "Script to be run when the media is ready to start playing"
            "onplaying", "Script to be run when the media actually has started playing"
            "onprogress", "Script to be run when the browser is in the process of getting the media data"
            "onratechange", "Script to be run each time the playback rate changes (like when a user switches to a slow motion or fast forward mode)"
            "onseeked", "Script to be run when the seeking attribute is set to false indicating that seeking has ended"
            "onseeking", "Script to be run when the seeking attribute is set to true indicating that seeking is active"
            "onstalled", "Script to be run when the browser is unable to fetch the media data for whatever reason"
            "onsuspend", "Script to be run when fetching the media data is stopped before it is completely loaded for whatever reason"
            "ontimeupdate", "Script to be run when the playing position has changed (like when the user fast forwards to a different point in the media)"
            "onvolumechange", "Script to be run each time the volume is changed which (includes setting the volume to mute)"
            "onwaiting", "Script to be run when the media has paused but is expected to resume (like when the media pauses to buffer more data)"
        ]

        miscEvents = [
            // "ontoggle", "Fires when the user opens or closes the <details> element"
        ]
    |}

let globalEvents =
    [
        eventDefinitions.formEvents
        eventDefinitions.keyboardEvents
        eventDefinitions.mouseEvents
        eventDefinitions.dragEvents
        eventDefinitions.clipboardEvents
        eventDefinitions.mediaEvents
        eventDefinitions.miscEvents
    ]
    |> List.collect id
    |> List.map (fun (n,d) -> { name = n; desc = d })

let (</>) (url1: string) (url2: string) =
    let del = '/'
    let sdel = string del
    let norm (s: string) = s.Trim().Replace(@"\", sdel)
    let delTrim = [| del |]
    let a = (norm url1).TrimEnd(delTrim)
    let b = (norm url2).TrimStart(delTrim).TrimEnd(delTrim)
    a + sdel + b

let generate () =
    let loadPage (relUrl: string) =
        let relUrl = "/tags" </> relUrl
        let cachePath = cacheDir </> (relUrl + ".html")
        if not (File.Exists(cachePath)) then
            let url = w3sRoot </> relUrl
            printfn $"Downloading {url} ..."
            get url
            |> Request.send
            |> Response.saveFile cachePath
        printfn $"LOADING URL: {relUrl}"
        use fs = File.OpenRead(cachePath)
        HtmlDocument.Load(fs).Body()
    let tryFindTable firstCellValue (node: HtmlNode) =
        node.CssSelect(".ws-table-all") 
        |> List.tryFind (fun t -> firstCellValue = (t.CssSelect("th")[0]).InnerText())
        |> Option.map (fun t -> t.CssSelect("tr")[1..])
    let findTable firstCellValue (node: HtmlNode) =
        (tryFindTable firstCellValue node).Value
    let getHRef (a: HtmlNode) = 
        a.Attribute("href").Value()
    let getNameAndMaybeLink (td: HtmlNode) =
        match td.CssSelect("a") |> List.tryExactlyOne with
        | Some a -> a.InnerText(), Some (getHRef a)
        | None -> td.InnerText(), None
    let getNameAndLink (td: HtmlNode) =
        let x,y = getNameAndMaybeLink td
        x, y.Value
    let correctName altNames name =
        altNames 
        |> List.tryFind (fun x -> fst x = name)
        |> Option.map snd
        |> Option.defaultValue name

    let scrapeAttrs (elemTagName: string option) (elementPage: HtmlNode) =
        let attrRows = elementPage |> tryFindTable "Attribute"|> Option.defaultValue []
        [ for attrTr in attrRows do
            let attrTds = attrTr.CssSelect("td")
            let attrName,attrLink = getNameAndMaybeLink (attrTds[0])
            if attrExcludes |> List.contains attrName |> not then
                let attrDesc = attrTds |> List.last |> fun x -> x.InnerText()
                let attrTypes =
                    match attrLink with
                    | None -> [AttrTyp.Text]
                    | Some attrLink ->
                        let attrPage = loadPage attrLink
                        let innerText = attrPage.InnerText().ToLowerInvariant()
                        let isBoolean = innerText.Contains("attribute is a boolean attribute")
                        let booleanAttrStyle = if innerText.Contains("when present") then Present else TrueFalse
                        let attrType =
                            match attrPage |> tryFindTable "Value" with
                            | None -> AttrTyp.Boolean booleanAttrStyle
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
                                    {
                                        Label.name = labelName
                                        fsharpName = 
                                            labelName
                                                .Replace("/", "_")
                                                .Replace("*", "_")
                                                .Replace(".", "_")
                                                // TODO: That's crap :)
                                        desc = labelDesc 
                                    }
                                ]
                                |> AttrTyp.Enum
                        [
                            if isBoolean then
                                AttrTyp.Boolean booleanAttrStyle
                            AttrTyp.Text
                            attrType
                        ]
                        |> List.distinct
                let setMode =
                    if 
                        domPropertySetter 
                        |> List.exists (fun (e,a) -> Some e = elemTagName && a = attrName)
                    then DomPropertySetter
                    else SetAttribute
                let finalAttr =
                    {
                        Attr.name = attrName
                        fsharpName = correctName attrNameCorrections attrName
                        desc = attrDesc
                        types = attrTypes
                        setMode = setMode
                        link = attrLink
                    }
                finalAttr
        ]
        |> List.sortBy (fun e -> e.name)

    let elements =
        [ 
            yield! additionalElements

            for elemTr in loadPage "default.asp" |> findTable "Tag" do
                let elemTds = elemTr.CssSelect("td")
                let elemTagName,elemLink =
                    let n,l = getNameAndLink (elemTds[0])
                    n.Replace("<", "").Replace(">", ""), l
                let elemTds = elemTr.CssSelect("td")
                let elemDesc = elemTds[1].InnerText()
                let elementPage = loadPage elemLink
                let isDeprecated = 
                    elementPage.CssSelect("span")
                    |> List.map (fun s -> s.InnerText())
                    |> List.exists (fun s -> s.Contains("Not Supported in HTML5", StringComparison.OrdinalIgnoreCase))
                if not (isDeprecated || elementExcludes |> List.contains elemTagName) then
                    let includeGlobalAttrs =
                        elementPage.CssSelect("a")
                        |> List.exists (fun a -> getHRef a = "ref_standardattributes.asp")
                    let includeGlobalEvents =
                        elementPage.CssSelect("a")
                        |> List.exists (fun a -> getHRef a = "ref_eventattributes.asp")
                    let attrs = scrapeAttrs (Some elemTagName) elementPage
                    let element =
                        {
                            Element.tagName = elemTagName
                            fsharpName = correctName elemNameCorrections elemTagName
                            elementType =
                                if voidElements |> List.contains elemTagName
                                then Void
                                else Content
                            returnsValue =
                                elementsWithReturnValue
                                |> List.contains elemTagName
                            domInterfaceName =  try domInterfaceMap[elemTagName] with _ -> failwith elemTagName
                            desc = elemDesc
                            link = elemLink
                            includeGlobalAttrs = includeGlobalAttrs
                            includeGlobalEvents = includeGlobalEvents
                            attrs = attrs
                            events = []
                        }
                    element
        ]
        |> List.sortBy (fun e -> e.tagName)
    {| 
        elements = elements
        globalEvents = globalEvents
        globalAttrs = 
            [
                yield! loadPage "ref_standardattributes.asp" |> scrapeAttrs None
                yield! additionalGlobalAttributes
            ]
    |}


let result = generate()

