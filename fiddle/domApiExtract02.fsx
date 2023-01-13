#r "nuget: Fable.Browser.Dom"

open System
open System.Reflection
open Browser
open Browser.Types
open Browser.Dom

let printTypes (t: Type seq) = t |> Seq.map (fun x -> x.Name) |> Seq.toList
let printProps (p: PropertyInfo seq) = 
    p 
    |> Seq.sortBy (fun x -> x.Name) 
    |> Seq.map (fun x -> $"{x.Name}:{x.PropertyType.Name}") 
    |> Seq.toList

let findDirectInheritors (t: Type) =
    t.Assembly.ExportedTypes
    |> Seq.filter (fun x -> x.BaseType = t || x.GetInterfaces() |> Array.contains t)
    |> Seq.toList

let baseElement = typeof<Element>
baseElement.Assembly.ExportedTypes

findDirectInheritors baseElement |> printTypes

typeof<HTMLAnchorElement>.GetInterfaces() |> printTypes

typeof<HTMLAnchorElement>.GetProperties() |> printProps
typeof<HTMLInputElement>.GetProperties() |> printProps
