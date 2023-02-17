namespace Vide.HtmlApiPreparation

// ==================================================================
// Fable <-> WebSharper
// --------------------
// - pascal case naming in "inputValue"
// - namespaces
// ==================================================================

open WebSharper.JavaScript
open WebSharper.JavaScript.Dom

type VoidResult = unit

[<AbstractClass>]
type NodeValue<'n>(node: 'n) =
    member _.Node = node

// TODO: "Value"s: Forward other (common) properties of the HTMLElements

type inputValue(node: HTMLInputElement) =
    inherit NodeValue<HTMLInputElement>(node)
    member _.TextValue
        with get() = node.Value
        and set(value) = node.Value <- value
    member _.IsChecked
        with get() = node.Checked
        and set(value) = node.Checked <- value

type datalistValue(node: HTMLDataListElement) =
    inherit NodeValue<HTMLDataListElement>(node)

type optionValue(node: HTMLOptionElement) =
    inherit NodeValue<HTMLOptionElement>(node)

type outputValue(node: HTMLElement) =
    inherit NodeValue<HTMLElement>(node)

type selectValue(node: HTMLSelectElement) =
    inherit NodeValue<HTMLSelectElement>(node)

type textareaValue(node: HTMLTextAreaElement) =
    inherit NodeValue<HTMLTextAreaElement>(node)

//type Event =
//    static member inline doBind(value: MutableValue<_>, getter) =
//        fun (args: Event.FableEventArgs<_, HTMLInputElement>) ->
//            value.Value <- getter(InputResult(args.node))
//    static member inline bind(value: MutableValue<string>) =
//        Event.doBind(value, fun x -> x.TextValue)
//    static member inline bind(value: MutableValue<int>) =
//        Event.doBind(value, fun x -> x.IntValue)
//    static member inline bind(value: MutableValue<float>) =
//        Event.doBind(value, fun x -> x.FloatValue)
//    static member inline bind(value: MutableValue<DateTime>) =
//        Event.doBind(value, fun x -> x.DateValue)
//    static member inline bind(value: MutableValue<bool>) =
//        Event.doBind(value, fun x -> x.IsChecked)
