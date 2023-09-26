#r "nuget: Avalonia,11.0.0"

open System
open System.Reflection
open Avalonia

module Fsi =
    let printProps (props: PropertyInfo seq) =
        props
        |> Seq.map (fun p -> $"{p.Name}: {p.PropertyType.Name}")
        |> Seq.iter (fun p -> printfn " - %s" p)
    let printPropsOfType (t: Type) =
        t.GetProperties() |> printProps

let lc,cc,pc =
    let controlType = typeof<Controls.Control>
    let contentControlType = typeof<Controls.ContentControl>
    let panelType = typeof<Controls.Panel>

    let isInstanciatable (t: Type) =
        not t.IsAbstract
        && t.GetConstructor([||]) <> null
    let isUsablePanelControl (t: Type) = 
        isInstanciatable t 
        && t.IsAssignableTo(panelType)
    let isUsableContentControl (t: Type) =
        isInstanciatable t
        && t.IsAssignableTo(contentControlType)
    let isUsableContentLeafControl (t: Type) =
        isInstanciatable t
        && t.IsAssignableTo(controlType)
        && not (isUsablePanelControl t)
        && not (isUsableContentControl t)

    let avaloniaTypes = typeof<Controls.StackPanel>.Assembly.ExportedTypes |> Seq.toList
    (
        avaloniaTypes |> List.filter isUsableContentLeafControl,
        avaloniaTypes |> List.filter isUsableContentControl,
        avaloniaTypes |> List.filter isUsablePanelControl
    )

// TODO: Multiple content / Content
// TODO: Animations
// TODO: Styles
// TODO: Attached properties

let t = typeof<Controls.Grid>
Fsi.printPropsOfType t
