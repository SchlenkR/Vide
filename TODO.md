
* "Resetting" views
* Optimize "EvaluateView"
* Events: Immer auch den Context mitgeben
* Perf
  * inline (if lambda)
  * diffing
  * instead of storing attr and events in the builder, they should have a direct effect on the underlying HTMLElement
* Samples
    * Conditional Attributes
    * Conditional Elements
    * Elements list + Remove from within element
    * State-Verschachtelungen (z.B. div in div mit jeweils State) oder State-In-List
* RawSpan
* All HTML API
* inputs
* Docu: Das geht so nicht - Trigger erklären!
        let! currentItems = Mutable.ofValue (ResizeArray())
        let addItem item = currentItems.value.Add(item)
        let removeItem item = currentItems.value.Remove(item) |> ignore
* Docu: Operators (+=, :=,. etc)        
* start: resolve issue "eventRemovalWorkaround with mutableLists example"
* Input, checkbox, etc.
* Vide as no single case DU -> inlineIfLambda
* Module / NS restructuring
* Access HTMLElement from builder wirh let! or via map/iter
* SVG API


----------------------------

* onRender (direct html element access), onMount, onUnmount
* timer / Observables
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

* reset component / state from outside
* HTML Api
    * globals as base class
* Way of emitting HTML (not only text)
* "Placeholder": A box that can be used later to place elements into

--------

Docu: We need "nothing" (that's F#)
    div.className("the-message") {
        nothing
    }


-----

# HTML Api Gen

* Don't call just ToString for attr values
* Enum types
* Choice types
* boolean enums ("hidden")
* events not as hardcoded list
* type and member docu
* elem.attrs.attrs |> List.distinctBy (fun a -> a.name)
