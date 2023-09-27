
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

type WrappableControl =
    { 
        typ: Type
        kind: ControlKind 
    }

module Type =
    let rec mkName proj (t: Type) =
        match t.GenericTypeArguments with
        | [| |] -> proj t
        | args ->
            let args = args |> Array.map (mkName proj) |> String.concat ", "
            let tyNameWithoutApostrophe = (proj t).Split("`")[0]
            $"""{tyNameWithoutApostrophe}<{args}>"""
    let rec getName (t: Type) = mkName (fun t -> t.Name) t
    let rec getFullName (t: Type) = mkName (fun t -> t.FullName) t

    let toWrappedControl (t: Type) =
        let controlType = typeof<Controls.Control>
        let contentControlType = typeof<Controls.ContentControl>
        let panelType = typeof<Controls.Panel>

        let isInstanciatableAndNonGeneric =
            not t.IsAbstract
            && t.GetConstructor([||]) <> null
        let isUsablePanelControl = 
            isInstanciatableAndNonGeneric
            && t.IsAssignableTo(panelType)
        let isUsableContentControl =
            isInstanciatableAndNonGeneric
            && t.IsAssignableTo(contentControlType)
        let isUsableContentLeafControl=
            isInstanciatableAndNonGeneric
            && t.IsAssignableTo(controlType)
            && not isUsablePanelControl
            && not isUsableContentControl
        
        if t.IsGenericType then None
        elif isUsableContentLeafControl then Some (LeafControl Ret)
        elif isUsableContentControl then Some (ContentControl Ret)
        elif isUsablePanelControl then Some PanelControl
        else None
        |> Option.map (fun kind -> { typ = t; kind = kind })

    let getProperties (t: Type) =
        t.GetProperties()
        |> Seq.filter (fun p -> 
            p.CanWrite && p.SetMethod <> null && p.SetMethod.IsPublic
            && p.CanRead && p.GetMethod <> null && p.GetMethod.IsPublic
            && p.GetIndexParameters().Length = 0
        )
        |> Seq.map (fun p -> {| name = p.Name; propType = p.PropertyType |})
        |> Seq.toList

    let getEvents (t: Type) =
        let methods = t.GetMethods(BindingFlags.Public ||| BindingFlags.Instance)
        [
            for m in methods do
                // OMG that's all so dirty.
                // But we generate code that will be compiled afterwards.
                match m.GetParameters() with
                | [| arg1 |] when 
                    arg1.ParameterType.GenericTypeArguments.Length = 1
                    && (m.Name.StartsWith("add_") || m.Name.StartsWith("remove_"))
                    ->
                        let evtName = m.Name.Split("_").[1]
                        let evtHandlerType = m.GetParameters().[0].ParameterType
                        {| name = evtName; eventHandlerType = evtHandlerType |}

                | _ -> ()
        ]

    let getAttachedProperties (t: Type) =
        [
            let attachedPropType = typedefof<Avalonia.AttachedProperty<_>>
            for f in t.GetFields(BindingFlags.Public ||| BindingFlags.Static) do
                if 
                    f.FieldType.GenericTypeArguments.Length > 0 
                    && f.FieldType.GetGenericTypeDefinition() = attachedPropType
                then
                    let ap = f.GetValue(null) :?> Avalonia.Data.Core.IPropertyInfo
                    if ap.CanSet then
                        yield {| name = ap.Name; propName = f.Name |}
        ]

module Model =
    let mkTemplateModelForAPs t =
        let aps = Type.getAttachedProperties t
        Tmpl.apOwner(
            t.FullName,
            [
                for ap in aps do
                    Tmpl.ap(ap.name, ap.propName)
            ],
            t.Name
        )

    let mkTemplateModelForType wc =
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
                getCtor mode, $"ContentLeaf{getBuilderInfixName mode}Builder", getBuilderPotGenArg mode
            | ContentControl mode ->
                getCtor mode, $"ContentLeaf{getBuilderInfixName mode}Builder", getBuilderPotGenArg mode
            | PanelControl ->
                "", "PanelRetBuilder", ""
        Tmpl.control(
            wc.typ.Name,
            ctor,
            potGenArg,
            builderName,
            wc.typ.FullName
        )

    let mkTemplateModelForTypes (wrappedTypes: WrappableControl list) =
        let controlModels =
            [ for wc in wrappedTypes do mkTemplateModelForType wc ]
        let apModels =
            [
                for wc in wrappedTypes do
                    let ap = mkTemplateModelForAPs wc.typ
                    if ap.properties.Length = 0 then None else Some ap
            ]
            |> List.choose id
        let propertyModels =
            [ for wc in wrappedTypes do yield! Type.getProperties wc.typ ]
            |> List.distinctBy (fun p -> p.name, p.propType.FullName)
            |> List.sortBy (fun x -> x.name)
            |> List.map (fun p -> Tmpl.prop(p.name, Type.getFullName p.propType))
        let eventModels =
            [ for wc in wrappedTypes do yield! Type.getEvents wc.typ ]
            |> List.distinct
            |> List.sortBy (fun x -> x.name)
            |> List.map (fun e -> Tmpl.evt(Type.getFullName e.eventHandlerType, e.name))

        Tmpl.Root(apModels, controlModels, eventModels, propertyModels)

let writeTemplate outFile templateModel =
    let renderedTemplate = Tmpl.Render templateModel
    if File.Exists(outFile) then
        File.Delete(outFile)
    File.WriteAllText(outFile, renderedTemplate)

let augmentType (controlTypeAugmentations) (wc: WrappableControl) =
    controlTypeAugmentations
    |> List.tryFind (fun (wcAug,_) -> wcAug.typ = wc.typ)
    |> Option.map (fun (wc,p) ->
        let prop = wc.typ.GetProperty(p)
        let pot = Pot {| propName = prop.Name; propTypeName = Type.getFullName prop.PropertyType |}
        { wc with kind = LeafControl pot }
    )
    |> Option.defaultValue wc

let genApiForWrappedControls augmentations outPath wcs =
    wcs
    |> List.map (augmentType augmentations)
    |> Model.mkTemplateModelForTypes
    |> writeTemplate outPath

let genApiForGivenTypes augmentations outPath types =
    types
    |> List.map Type.toWrappedControl
    |> List.map (fun x -> x.Value)
    |> genApiForWrappedControls outPath augmentations

let getWrappedTypesInAssembly (asm: Assembly) =
    asm.ExportedTypes
    |> List.ofSeq
    |> List.map Type.toWrappedControl
    |> List.choose id

let genApiForAllTypesInAssembly augmentations outPath (asm: Assembly) =
    getWrappedTypesInAssembly asm
    |> genApiForWrappedControls outPath augmentations


// -------------------------


let commonOutPath = Path.Combine(__SOURCE_DIRECTORY__, "../../Vide.UI.Avalonia/Api.fs")

let commonAugmentations =
    [
        typeof<Controls.TextBox>, Controls.TextBox.TextProperty.Name
        typeof<Controls.CheckBox>, Controls.CheckBox.IsCheckedProperty.Name
    ]
    |> List.map (fun (t,p) -> (Type.toWrappedControl t).Value, p)



genApiForAllTypesInAssembly 
    commonOutPath 
    commonAugmentations 
    typeof<Controls.Control>.Assembly


let private FSI_TEST () =
        
    let testTypes = 
            [
            typeof<Controls.TextBlock>
            typeof<Controls.TextBox>
            typeof<Controls.Button>
            typeof<Controls.CheckBox>
            typeof<Controls.Grid>
            typeof<Controls.DockPanel>
            typeof<Controls.StackPanel>
            typeof<Controls.ScrollViewer>
        ]
    
    genApiForGivenTypes 
        commonOutPath 
        commonAugmentations 
        testTypes

    genApiForAllTypesInAssembly 
        commonOutPath 
        commonAugmentations 
        typeof<Controls.Control>.Assembly


    let getClassHierarchy (t: Type) =
        let stopType = typeof<AvaloniaObject>
        let rec loop (t: Type) =
            match t.BaseType with
            | null -> []
            | bt when bt = stopType -> [stopType]
            | bt ->bt :: loop bt
        loop t
    
    let printTypes types =
        for t in types do printfn "%s" (Type.getFullName t)

    getClassHierarchy typeof<Controls.DockPanel> |> printTypes
    
    getWrappedTypesInAssembly typeof<Controls.Control>.Assembly
    |> List.map (fun x -> x.typ)
    |> printTypes

    [
        // typeof<Avalonia.Controls.AutoCompleteBox>
        typeof<Avalonia.Controls.Border>
        typeof<Avalonia.Controls.Button>
        // typeof<Avalonia.Controls.ButtonSpinner>
        // typeof<Avalonia.Controls.CalendarDatePicker>
        // typeof<Avalonia.Controls.Calendar>
        typeof<Avalonia.Controls.Canvas>
        // typeof<Avalonia.Controls.Carousel>
        typeof<Avalonia.Controls.CheckBox>
        typeof<Avalonia.Controls.ComboBox>
        // typeof<Avalonia.Controls.ComboBoxItem>
        typeof<Avalonia.Controls.ContentControl>
        // typeof<Avalonia.Controls.ContextMenu>
        typeof<Avalonia.Controls.Control>
        // typeof<Avalonia.Controls.DataValidationErrors>
        typeof<Avalonia.Controls.DatePicker>
        // typeof<Avalonia.Controls.DatePickerPresenter>
        typeof<Avalonia.Controls.TimePicker>
        // typeof<Avalonia.Controls.TimePickerPresenter>
        // typeof<Avalonia.Controls.Decorator>
        typeof<Avalonia.Controls.DockPanel>
        // typeof<Avalonia.Controls.DropDownButton>
        typeof<Avalonia.Controls.Expander>
        // typeof<Avalonia.Controls.ExperimentalAcrylicBorder>
        // typeof<Avalonia.Controls.FlyoutPresenter>
        // typeof<Avalonia.Controls.MenuFlyoutPresenter>
        typeof<Avalonia.Controls.Grid>
        // typeof<Avalonia.Controls.GridSplitter>
        // typeof<Avalonia.Controls.Image>
        // typeof<Avalonia.Controls.ItemsControl>
        typeof<Avalonia.Controls.Label>
        // typeof<Avalonia.Controls.LayoutTransformControl>
        // typeof<Avalonia.Controls.ListBox>
        // typeof<Avalonia.Controls.ListBoxItem>
        // typeof<Avalonia.Controls.MaskedTextBox>
        // typeof<Avalonia.Controls.Menu>
        // typeof<Avalonia.Controls.MenuItem>
        // typeof<Avalonia.Controls.NativeControlHost>
        // typeof<Avalonia.Controls.NativeMenuBar>
        // typeof<Avalonia.Controls.ReversibleStackPanel>
        // typeof<Avalonia.Controls.NumericUpDown>
        // typeof<Avalonia.Controls.Panel>
        // typeof<Avalonia.Controls.PathIcon>
        // typeof<Avalonia.Controls.ProgressBar>
        // typeof<Avalonia.Controls.RefreshContainer>
        // typeof<Avalonia.Controls.RefreshVisualizer>
        // typeof<Avalonia.Controls.RadioButton>
        // typeof<Avalonia.Controls.RelativePanel>
        // typeof<Avalonia.Controls.RepeatButton>
        // typeof<Avalonia.Controls.ScrollViewer>
        // typeof<Avalonia.Controls.SelectableTextBlock>
        // typeof<Avalonia.Controls.Separator>
        // typeof<Avalonia.Controls.Slider>
        // typeof<Avalonia.Controls.SplitButton>
        // typeof<Avalonia.Controls.ToggleSplitButton>
        // typeof<Avalonia.Controls.SplitView>
        typeof<Avalonia.Controls.StackPanel>
        // typeof<Avalonia.Controls.TabControl>
        // typeof<Avalonia.Controls.TabItem>
        typeof<Avalonia.Controls.TextBlock>
        typeof<Avalonia.Controls.TextBox>
        // typeof<Avalonia.Controls.ThemeVariantScope>
        // typeof<Avalonia.Controls.TickBar>
        // typeof<Avalonia.Controls.ToggleSwitch>
        // typeof<Avalonia.Controls.ToolTip>
        // typeof<Avalonia.Controls.TransitioningContentControl>
        // typeof<Avalonia.Controls.TreeView>
        // typeof<Avalonia.Controls.TreeViewItem>
        // typeof<Avalonia.Controls.UserControl>
        // typeof<Avalonia.Controls.Viewbox>
        // typeof<Avalonia.Controls.VirtualizingCarouselPanel>
        // typeof<Avalonia.Controls.VirtualizingStackPanel>
        // typeof<Avalonia.Controls.Window>
        typeof<Avalonia.Controls.WrapPanel>
        // typeof<Avalonia.Controls.Shapes.Arc>
        // typeof<Avalonia.Controls.Shapes.Ellipse>
        // typeof<Avalonia.Controls.Shapes.Line>
        // typeof<Avalonia.Controls.Shapes.Path>
        // typeof<Avalonia.Controls.Shapes.Polygon>
        // typeof<Avalonia.Controls.Shapes.Polyline>
        // typeof<Avalonia.Controls.Shapes.Rectangle>
        // typeof<Avalonia.Controls.Shapes.Sector>
        // typeof<Avalonia.Controls.Presenters.ContentPresenter>
        // typeof<Avalonia.Controls.Presenters.ItemsPresenter>
        // typeof<Avalonia.Controls.Presenters.ScrollContentPresenter>
        // typeof<Avalonia.Controls.Presenters.TextPresenter>
        // typeof<Avalonia.Controls.Notifications.NotificationCard>
        // typeof<Avalonia.Controls.Embedding.EmbeddableControlRoot>
        // typeof<Avalonia.Controls.Chrome.CaptionButtons>
        // typeof<Avalonia.Controls.Chrome.TitleBar>
        // typeof<Avalonia.Controls.Primitives.CalendarButton>
        // typeof<Avalonia.Controls.Primitives.CalendarDayButton>
        // typeof<Avalonia.Controls.Primitives.CalendarItem>
        // typeof<Avalonia.Controls.Primitives.DateTimePickerPanel>
        // typeof<Avalonia.Controls.Primitives.AccessText>
        // typeof<Avalonia.Controls.Primitives.AdornerLayer>
        // typeof<Avalonia.Controls.Primitives.ChromeOverlayLayer>
        // typeof<Avalonia.Controls.Primitives.HeaderedContentControl>
        // typeof<Avalonia.Controls.Primitives.HeaderedItemsControl>
        // typeof<Avalonia.Controls.Primitives.HeaderedSelectingItemsControl>
        // typeof<Avalonia.Controls.Primitives.LightDismissOverlayLayer>
        // typeof<Avalonia.Controls.Primitives.OverlayLayer>
        // typeof<Avalonia.Controls.Primitives.Popup>
        // typeof<Avalonia.Controls.Primitives.ScrollBar>
        // typeof<Avalonia.Controls.Primitives.SelectingItemsControl>
        // typeof<Avalonia.Controls.Primitives.TabStrip>
        // typeof<Avalonia.Controls.Primitives.TabStripItem>
        // typeof<Avalonia.Controls.Primitives.TemplatedControl>
        // typeof<Avalonia.Controls.Primitives.Thumb>
        // typeof<Avalonia.Controls.Primitives.ToggleButton>
        // typeof<Avalonia.Controls.Primitives.Track>
        // typeof<Avalonia.Controls.Primitives.UniformGrid>
        // typeof<Avalonia.Controls.Primitives.VisualLayerManager>
    ]
    |> genApiForGivenTypes commonOutPath commonAugmentations 
