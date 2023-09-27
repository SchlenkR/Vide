#r "nuget: Avalonia"

open System
open System.Reflection
open Avalonia.Interactivity

type MyType() =
    let click = Event<EventArgs>()

    [<CLIEvent>]
    member this.Click = click.Publish
    member this.OnClick() = click.Trigger(EventArgs())

let inline handleClick<'a when 'a: (member Click: IEvent<EventArgs>)> handler (x: 'a) =
    Event.add handler x.Click

let myType = MyType()
do handleClick (fun _ -> printfn "clicked") myType
myType.OnClick()



let inline handleClickAvalonia<'a,'args when 'a: (member add_Click: EventHandler<'args> -> unit)> handler (x: 'a) =
    x.add_Click(EventHandler<_>(handler))

let btn = Avalonia.Controls.Button()

handleClickAvalonia (fun _ _ -> printfn "clicked") btn





let rec getTypeName (t: Type) =
    match t.GenericTypeArguments with
    | [| |] -> t.Name
    | args ->
        let args = args |> Array.map getTypeName |> String.concat ", "
        let tyNameWithoutApostrophe = t.Name.Split("`")[0]
        $"""{tyNameWithoutApostrophe}<{args}>"""

let printMembers (t: Type) =
    t.GetMembers(
        BindingFlags.Public
        ||| BindingFlags.Instance
        ||| BindingFlags.DeclaredOnly)
    |> Array.sortBy (fun x -> x.Name)
    |> Array.iter (fun x -> printfn $"{x.Name} (typ = {x.MemberType})")

let printMethods (t: Type) =
    t.GetMethods(
        BindingFlags.Public 
        ||| BindingFlags.Instance
        ||| BindingFlags.DeclaredOnly)
    |> Array.iter (fun x ->
        let args = 
            x.GetParameters() 
            |> Array.map (fun x -> $"({x.Name}: {getTypeName x.ParameterType})")
            |> String.concat "; "
        printfn $"{x.Name} (typ = {x.MemberType}) (args = [ {args} ]) : {getTypeName x.ReturnType}")

printMethods typeof<Avalonia.Controls.Button>




type Controls = class end
type Controls with static member DoIt() = ()
type Controls with static member DoIt2() = ()





type MyPropType() =
    member val Margin = 100 with get, set

let inline setMargin<^a 
        when ^a: (member get_Margin: unit -> int)
        and ^a: (member set_Margin: int -> unit)
    > (this: ^a) value
    =
    this.set_Margin(value)
    let margin = this.get_Margin()
    printfn $"margin = {margin}"

let inst = MyPropType()
setMargin inst 333

