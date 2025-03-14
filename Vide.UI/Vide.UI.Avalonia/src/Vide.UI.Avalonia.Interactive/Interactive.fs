module Vide.UI.Avalonia.Interactive

open Vide

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Avalonia.Threading

open FSharp.Compiler.Interactive

let mutable private isInitialized = false
let mutable private isEventLoopRunning = false

type App<'v,'s>() =
    inherit Application()
    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

let guardInit () =
    if not isInitialized then
        do isInitialized <- true
        printfn "Initializing Avalonia event loop ..."
        fsi.EventLoop <-
            { new IEventLoop with 
                member _.Run() =
                    do 
                        isEventLoopRunning <- true
                        printfn "Avalonia event loop running!"
                    do
                        AppBuilder
                            .Configure(fun () -> App())
                            .UsePlatformDetect()
                            .UseSkia()
                            .StartWithClassicDesktopLifetime([||], ShutdownMode.OnExplicitShutdown)
                        |> ignore
                    false
                member _.Invoke(f) =
                    Dispatcher.UIThread.Invoke(System.Func<'a>(fun () -> f ()))
                member _.ScheduleRestart() = ()
            }

let createWindow width height =
    if not isEventLoopRunning then
        failwith "Avalonia event loop is not running yet. Split the first evaluation with #r and a call to 'ensureAvalonia' from subsequent evaluations."
    match Application.Current.ApplicationLifetime with
    | :? IClassicDesktopStyleApplicationLifetime ->
        let window = Window(
            Background = Media.Brushes.DarkSlateBlue,
            Width = width,
            Height = height,
            WindowStartupLocation = WindowStartupLocation.CenterOwner)
        do window.Show()
        window
    | null -> failwith $"ApplicationLifetime is null"
    | alt -> failwith $"Unexpected ApplicationLifetime: {alt.GetType().FullName}"

let showView videView (host: ContentControl) =
    let videApp = VideApp.ForHost(host).CreateAndStart(videView)
    videApp
    
[<AutoOpen>]
module Dynamic =
    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection

    // Various flags that specify what members can be called 
    // NOTE: Remove 'BindingFlags.NonPublic' if you want a version
    // that can call only public methods of classes
    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static 
    let instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let private ctorFlags = instanceFlags
    let inline asMethodBase(a:#MethodBase) = a :> MethodBase

    // The operator takes just instance and a name. Depending on how it is used
    // it either calls method (when 'R is function) or accesses a property
    let (?) (o:obj) name : 'R =
        // The return type is a function, which means that we want to invoke a method
        if FSharpType.IsFunction(typeof<'R>) then
    
            // Get arguments (from a tuple) and their types
            let argType, resType = FSharpType.GetFunctionElements(typeof<'R>)
            // Construct an F# function as the result (and cast it to the
            // expected function type specified by 'R)
            FSharpValue.MakeFunction(typeof<'R>, fun args ->
            
                // We treat elements of a tuple passed as argument as a list of arguments
                // When the 'o' object is 'System.Type', we call static methods
                let methods, instance, args = 
                    let args = 
                        // If argument is unit, we treat it as no arguments,
                        // if it is not a tuple, we create singleton array,
                        // otherwise we get all elements of the tuple
                        if argType = typeof<unit> then [| |]
                        elif not(FSharpType.IsTuple(argType)) then [| args |]
                        else FSharpValue.GetTupleFields(args)
        
                    // Static member call (on value of type System.Type)?
                    if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then 
                        let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
                        let ctors = (unbox<Type> o).GetConstructors(ctorFlags) |> Array.map asMethodBase
                        Array.concat [ methods; ctors ], null, args
                    else 
                        o.GetType().GetMethods(instanceFlags) |> Array.map asMethodBase, o, args
                
                // A simple overload resolution based on the name and the number of parameters only
                // TODO: This doesn't correctly handle multiple overloads with same parameter count
                let methods = 
                    [ for m in methods do
                        if m.Name = name && m.GetParameters().Length = args.Length then yield m ]
                
                // If we find suitable method or constructor to call, do it!
                match methods with 
                | [] -> failwithf "No method '%s' with %d arguments found" name args.Length
                | _::_::_ -> failwithf "Multiple methods '%s' with %d arguments found" name args.Length
                | [:? ConstructorInfo as c] -> c.Invoke(args)
                | [ m ] -> m.Invoke(instance, args) ) |> unbox<'R>
        else
            // The result type is not an F# function, so we're getting a property
            // When the 'o' object is 'System.Type', we access static properties
            let typ, flags, instance = 
                if (typeof<System.Type>).IsAssignableFrom(o.GetType()) 
                then unbox o, staticFlags, null
                else o.GetType(), instanceFlags, o
            
            // Find a property that we can call and get the value
            let prop = typ.GetProperty(name, flags)
            if prop = null && instance = null then 
                // The syntax can be also used to access nested types of a type
                let nested = typ.Assembly.GetType(typ.FullName + "+" + name)
                // Return nested type if we found one
                if nested = null then 
                    failwithf "Property or nested type '%s' not found in '%s'." name typ.Name 
                elif not ((typeof<'R>).IsAssignableFrom(typeof<System.Type>)) then
                    let rname = (typeof<'R>.Name)
                    failwithf "Cannot return nested type '%s' as a type '%s'." nested.Name rname
                else nested |> box |> unbox<'R>
            else
                // Call property and return result if we found some
                let meth = prop.GetGetMethod(true)
                if prop = null then failwithf "Property '%s' found, but doesn't have 'get' method." name
                try meth.Invoke(instance, [| |]) |> unbox<'R>
                with _ -> failwithf "Failed to get value of '%s' property (of type '%s')" name typ.Name
    
// TODO
// let printState (s: obj) =
//     let rec prec indentation (s: obj) =
//         try
//             if s = null then
//                 printfn "%snull" indentation
//             elif s.GetType().FullName.StartsWith("Microsoft.FSharp.Core.FSharpOption`1[[System.Tuple`2") then
//                 try
//                     let tuple = s?Value
//                     let item1 = tuple?Item1
//                     let item2 = tuple?Item2
//                     printfn "%A" item1
//                     prec (indentation + "  ") item2
//                 with _ ->
//                     printfn "%sNone" indentation
//             else
//                 printfn "%s%s" indentation (s.ToString())
//         with e ->
//             printfn "%s%s" indentation (e.ToString())
//     prec "" s

// printState videApp.CurrentState
