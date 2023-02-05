namespace Vide.HtmlApiPreparation

open Browser.Types

type VoidResult = unit

// TODO: All other input possibilities
type inputValue(node: HTMLInputElement) =
    member _.Node = node
    member _.TextValue = node.value
    member _.DateValue = node.valueAsDate
    member _.FloatValue = node.valueAsNumber
    member _.IntValue = node.valueAsNumber |> int
    member _.IsChecked = node.checked

type datalistValue(node: HTMLDataListElement) =
    class end

type optionValue(node: HTMLOptionElement) =
    class end

type outputValue(node: HTMLElement) =
    class end

type selectValue(node: HTMLSelectElement) =
    class end

type textareaValue(node: HTMLTextAreaElement) =
    class end

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
