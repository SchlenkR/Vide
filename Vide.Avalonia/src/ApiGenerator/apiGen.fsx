#r "nuget: Avalonia,11.0.0"

open System
open Avalonia

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
