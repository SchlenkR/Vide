
Mandatory
---

* AttachedProperties (Wpfish)
* Solution for multiple content properties / templates + slots


New Ideas / Brainstorm
---

* onRender (direct html element access), onMount, onUnmount
* timer / Observables
* SVG API
* Vide als .fsx file und dann einen C# SourceGen


Useful (already discovered)
---

* "Resetting" views
* RawSpan / Way of emitting HTML (not only text)
* async
    * Restart- und Retrigger-Verhalten
    * CancellationToken
    * Error handling
    * Timeouts
    * async button ("wait for click")
    * Maybe don't use later/promise, but async because of cancellation
    * UseCase: "Until" (geht schon jetzt)
    * UseCase: See AsyncWithSubsequentCalls
        // TODO: Interesting use case for component result
        // handling and ordering / evaluation
* Components
    * reset component / state from outside
    * Templates / slots
    * Provide an "element" function
    * yield support for fragments
        <>
            <....
        <>
    * Welche Arten?
        * CompoundComponents (=vide { ... })
        * RenderComponents (die die direkt auf ctx zugreuif)
    * Gutes Beispiel: Aus "input" eine "checkbox"-Komponente machen
    * Components with events


Performance / Optimizations / Robustheit
---

* Optimize "EvaluateView"
* Perf
  * inline (if lambda)
  * diffing
  * instead of storing attr and events in the builder, they should have a direct effect on the underlying HTMLElement
* MemLeaks bei evt reg?
* Testcase:
    For-Loop mit disjunktem State -> wie verhält sich das?


Samples
---
* Conditional Attributes
* Conditional Elements
* Elements list + Remove from within element
* State-Verschachtelungen (z.B. div in div mit jeweils State) oder State-In-List



Propably not
---
* Vide as no single case DU -> inlineIfLambda




----------------------------



* HTML Api
    * globals as base class
    * "add className"
    * className + class'
    * Properties / Ctor initialization
        type X(?myProp: int) =
            let mutable _myProp = defaultArg myProp 0
            member this.myProp(xxx) = _myProp <- xxx
            member this.myProp() = _myProp
    * Other events like checked change
    * hr, br etc. sollen Void sein
    * Events:
        * Overload ohne Argumente (Evaluiert nur)
        * Event soll ein Arg bekommen mit
            - Event (Fable)
            - Node
            - Eventkontext mit "TriggerEval: bool"
    * Input soll automatisch "OnChange" auslösen
    * Kontext weiter abstrahieren, damit man ohne Browser testen kann
    * input: checked / radio / etc.
    * form elements

* "Placeholder": A box that can be used later to place elements into


HTML Api Gen
---

* Don't call just ToString for attr values
* Enum types
* Choice types
* events not as hardcoded list
* type and member docu
* elem.attrs.attrs |> List.distinctBy (fun a -> a.name)
*
    let x1 = X()config..myProp()
    let x2 = X(myProp = 12).attrs.myProp()
    let x2 = X(myProp = 12).attrs.myProp()

