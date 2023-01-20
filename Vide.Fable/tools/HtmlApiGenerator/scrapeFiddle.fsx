#load "w3schoolScrape.fsx"
open W3schoolScrape
open System.IO

// from: http://www.fssnip.net/3U/title/CSV-writer
module Csv =
    open Microsoft.FSharp.Reflection

    type Array =
        static member join delimiter xs = 
            xs 
            |> Array.map (fun x -> x.ToString())
            |> String.concat delimiter

    let toText (separator: string) (useEnclosure: bool) (headerMapping: string -> string) (data: seq<'a>) =
        seq {
            let dataType = typeof<'a>
            let stringSeqDataType = typeof<System.Collections.Generic.IEnumerable<string>>
            let inline enclose s =
                match useEnclosure with
                | true -> "\"" + (string s) + "\""
                | false -> string s

            let header = 
                match dataType with
                | ty when FSharpType.IsRecord ty ->
                    FSharpType.GetRecordFields dataType
                    |> Array.map (fun info -> headerMapping info.Name)                    
                | ty when FSharpType.IsTuple ty -> 
                    FSharpType.GetTupleElements dataType
                    |> Array.mapi (fun idx info -> headerMapping(string idx) )
                | ty when ty.IsAssignableFrom stringSeqDataType ->
                    data :?> seq<seq<string>> |> Seq.head
                    |> Seq.toArray
                | _ -> dataType.GetProperties()
                    |> Array.map (fun info -> headerMapping info.Name)

            yield header |> Array.map enclose |> Array.join separator
                                    
            let lines =
                match dataType with 
                | ty when FSharpType.IsRecord ty -> 
                    data |> Seq.map FSharpValue.GetRecordFields
                | ty when FSharpType.IsTuple ty ->
                    data |> Seq.map FSharpValue.GetTupleFields
                | ty when ty.IsAssignableFrom stringSeqDataType ->
                    data :?> seq<seq<string>> |> Seq.tail
                    |> Seq.map (fun ss -> Seq.toArray ss |> Array.map (fun s -> s :> obj) )
                | _ -> 
                    let props = dataType.GetProperties()
                    data |> Seq.map ( fun line -> 
                                props |> Array.map ( fun prop ->
                                prop.GetValue(line, null) ))
                |> Seq.map (Array.map enclose)
            yield! lines |> Seq.map (Array.join separator)
        }
        |> String.concat "\n"

    let serialize (data: seq<'a>) = data |> toText "|" false id


let showType (this: AttrTyp) =
    let labelsToString (labels: Label list) =
        labels |> List.map (fun x -> x.name) |> String.concat ", "
    match this with
    | AttrTyp.Boolean -> "bool"
    | AttrTyp.Text -> "text"
    | AttrTyp.Enum labels -> $"Enum ({labelsToString labels})"

do
    [ for e in result.elements do
        for a in e.attrs do
            for t in a.types do
                {|
                    elementName = e.name
                    attrName = a.name
                    fsharpName = a.fsharpName
                    typ = showType t
                |}
    ]
    |> List.sortBy (fun x -> x.elementName, x.attrName)
    |> Csv.serialize 
    |> fun s -> File.WriteAllText("c:/temp/html/attributes.csv", s)
