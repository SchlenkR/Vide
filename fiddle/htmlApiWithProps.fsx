
open System.Runtime.CompilerServices

module HtmlElementBuilders =
    
    type a() =
        static member ctor : unit -> a = a
        member this.href 
            with get () = ""
            and set (value: string) = ()
    

open HtmlElementBuilders

[<Extension>]
type aExtensions =
    [<Extension>]
    static member href(this: unit -> a, value: string) =
        //fun () -> this().OnEval(fun x -> x.setAttribute("href", value))       
        fun () ->
            let this = this()
            this.href <- value

//type Html =
//    static member inline a = HtmlElementBuilders.a()
//    //static member inline a(value: string) = HtmlElementBuilders.a()


let x0 = a(href = "hello")

let x2 = a.ctor.href("xxx")
