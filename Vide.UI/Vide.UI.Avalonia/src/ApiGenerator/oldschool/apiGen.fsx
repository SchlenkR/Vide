
#load "template.fsx"

#r "nuget: Avalonia,11.0.0"
#r "nuget: trulla"

open Trulla
open System.IO

type Tmpl = Template<Template.ApiTextTemplate>

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

type ControlKind =
    | LeafControl of BuilderMode
    | ContentControl of BuilderMode
    | PanelControl
and BuilderMode =
    | Ret
    | Pot of {| propName: string; propTypeName: string |}

type WrappableControl = { typ: Type; kind: ControlKind }

let rec getTypeName (t: Type) =
    match t.GenericTypeArguments with
    | [| |] -> t.Name
    | args ->
        let args = args |> Array.map getTypeName |> String.concat ", "
        let tyNameWithoutApostrophe = t.Name.Split("`")[0]
        $"""{tyNameWithoutApostrophe}<{args}>"""

let toWrappedControl (t: Type) =
    let controlType = typeof<Controls.Control>
    let contentControlType = typeof<Controls.ContentControl>
    let panelType = typeof<Controls.Panel>

    let isInstanciatable=
        not t.IsAbstract
        && t.GetConstructor([||]) <> null
    let isUsablePanelControl = 
        isInstanciatable
        && t.IsAssignableTo(panelType)
    let isUsableContentControl =
        isInstanciatable
        && t.IsAssignableTo(contentControlType)
    let isUsableContentLeafControl=
        isInstanciatable
        && t.IsAssignableTo(controlType)
        && not isUsablePanelControl
        && not isUsableContentControl
    
    if isUsableContentLeafControl then Some (LeafControl Ret)
    elif isUsableContentControl then Some (ContentControl Ret)
    elif isUsablePanelControl then Some PanelControl
    else None
    |> Option.map (fun kind -> { typ = t; kind = kind })

let findWrappedControlsInAsm (asm: Assembly) =
    asm.ExportedTypes
    |> Seq.map toWrappedControl
    |> Seq.choose id
    |> Seq.toList

let getProperties (t: Type) =
    t.GetProperties()
    |> Seq.filter (fun p -> 
        p.CanWrite && p.SetMethod <> null && p.SetMethod.IsPublic
        && p.CanRead && p.GetMethod <> null && p.GetMethod.IsPublic
        && p.GetIndexParameters().Length = 0
    )

let getEvents (t: Type) =
    t.GetEvents()

let findAPsOnType (t: Type) =
    [
        let attachedPropType = typedefof<Avalonia.AttachedProperty<_>>
        for f in t.GetFields(BindingFlags.Public ||| BindingFlags.Static) do
            if 
                f.FieldType.GenericTypeArguments.Length > 0 
                && f.FieldType.GetGenericTypeDefinition() = attachedPropType 
            then
                yield f.GetValue(null) :?> Avalonia.Data.Core.IPropertyInfo
    ]

let findAPsInAssembly (asm: Assembly) =
    [
        for t in asm.GetTypes() do 
            for ap in findAPsOnType t do
                ap
    ]

// TODO: Multiple content / Content
// TODO: Animations
// TODO: Styles

let mkTemplateModelForAPs t =
    let aps = findAPsOnType t
    Tmpl.apOwner(
        t.FullName,
        [
            for ap in aps do
                Tmpl.ap(ap.Name)
        ],
        t.Name
    )

let mkTemplateModelForType wc potPropertyName =
    let getMode mode =
        match potPropertyName with 
        | Some prop ->
            let prop = wc.typ.GetProperty(prop)
            Pot {| propName = prop.Name; propTypeName = getTypeName prop.PropertyType |}
        | None -> mode
    let props = 
        [
            for p in getProperties wc.typ do
                Tmpl.prop(p.Name)
        ]
    let evts =
        [ 
            for e in getEvents wc.typ do
                Tmpl.evt(e.Name)
        ]
    let getCtor mode =
        match mode with
        | Ret -> ""
        | Pot prop -> $"fun node -> node.{prop.propName}"
    let getBuilderInfixName mode =
        match mode with
        | Ret -> "Ret"
        | Pot _ -> "Pot"
    let getBuilderPotGenArg mode =
        match mode with
        | Ret -> ""
        | Pot pot -> $"{pot.propTypeName}, "
    let ctor,builderName,potGenArg =
        match wc.kind with
        | LeafControl mode ->
            let mode = getMode mode
            getCtor mode, $"ContentLeaf{getBuilderInfixName mode}Builder", getBuilderPotGenArg mode
        | ContentControl mode ->
            let mode = getMode mode
            getCtor mode, $"ContentLeaf{getBuilderInfixName mode}Builder", getBuilderPotGenArg mode
        | PanelControl ->
            match potPropertyName with
            | Some _ -> failwith "PanelControl can't have a mode!"
            | _ -> ()
            "", "PanelRetBuilder", ""
    Tmpl.control(
        wc.typ.Name,
        ctor,
        evts,
        potGenArg,
        props,
        builderName,
        wc.typ.FullName
    )

let mkTemplateModelForTypes types =
    [
        for t,potPropName in types do
            let wc = 
                match toWrappedControl t with
                | Some wc -> wc
                | None -> failwith $"Can't wrap type {t.FullName}"
            let ap =
                let ap = mkTemplateModelForAPs wc.typ
                if ap.properties.Length = 0 then None else Some ap
            let tp = mkTemplateModelForType wc potPropName
            yield ap,tp
    ]
    |> List.unzip
    |> fun (aps,types) -> 
        let aps = aps |> List.choose id
        Tmpl.Root(aps, types)

let writeTemplate outFile templateModel =
    let renderedTemplate = Tmpl.Render templateModel
    if File.Exists(outFile) then
        File.Delete(outFile)
    File.WriteAllText(outFile, renderedTemplate)

let private FSI_TEST () =

    let controlsToWrap =
        [
            typeof<Controls.TextBlock>, None
            typeof<Controls.TextBox>, Some Controls.TextBox.TextProperty.Name
            typeof<Controls.Button>, None
            typeof<Controls.CheckBox>, Some Controls.CheckBox.IsCheckedProperty.Name
            typeof<Controls.Grid>, None
            typeof<Controls.DockPanel>, None
            typeof<Controls.StackPanel>, None
            typeof<Controls.ScrollViewer>, None
        ]
    
    controlsToWrap
    |> mkTemplateModelForTypes
    |> writeTemplate (Path.Combine(__SOURCE_DIRECTORY__, "../../Vide.UI.Avalonia/Api.fs"))

    let getClassHierarchy (t: Type) =
        let stopType = typeof<AvaloniaObject>
        let rec loop (t: Type) =
            match t.BaseType with
            | null -> []
            | bt when bt = stopType -> [stopType]
            | bt ->bt :: loop bt
        loop t
    let printTypes types =
        for t in types do printfn "%s" (getTypeName t)

    getClassHierarchy typeof<Controls.DockPanel> |> printTypes
    
