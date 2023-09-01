module UseCases.CustomElements

// see also: https://github.com/RonaldSchlenker/Vide/issues/2

module UntypedCustomElementExample =
    open Vide
    open type Vide.Html

    let untypedCustomElement =
        vide {
            div {
                (e "untyped-custom-elem")
                    .attr("untyped-custom-attribute", "Hello there")
                    .on("untyped-custom-event", fun evt -> ()) {
                        "Hello there again"
                }
            }
        }

module TypedCustomElementExample =
    
    module MyCustomElementDefinitions =
        open Browser.Types
        open Vide
        open Vide.HtmlElementBuilders

        type typedCustomElementBuilder() =
            inherit HtmlGARenderRetCnBuilder<HTMLElement>("typed-custom-element")
            member this.myAttribute(value: string) =
                this.attr("myAttribute", value)
            member this.myBooleanAttribute(value: bool) =
                this.attrBoolean("myBooleanAttribute", value)
            member this.onMyEvent(handler) =
                this.on("myEvent", handler)

        [<AutoOpen>]
        type MyCustomElements =
            static member inline typedCustomElement = typedCustomElementBuilder()
    
    module UsingTypedCustomElements =
        open Vide
        open type Vide.Html
        open MyCustomElementDefinitions

        let view = 
            vide {
                div {
                    typedCustomElement
                        .myBooleanAttribute(false)
                        .myAttribute("Hello")
                        .onMyEvent(fun evt -> ()) {
                            "typedCustomElement can have content because it inherits from HtmlGARenderRetCnBuilder."
                            "if you don't like that, inherit from HtmlGARenderRetC0Builder."
                        }
                }
            }