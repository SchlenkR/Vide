* Optimize "EvaluateView"
* Events: Immer auch den Context mitgeben
* let! ctx = context
* mutable lists with fine-grained updates
* Perf
  * inline (if lambda)
  * diffing
  * instead of storing attr and events in the builder, they should have a direct effect on the underlying HTMLElement
* API
  * attrsReplace / attrsAdd
* Da der Builder durch die HTML DSL selbst eine Art Kontext ist, könnte man das mit dem aktuellen Kontext vereinheitlichen
* Provide WPF API
* Idee: Die Builder abschaffen und direkt Vide verwenden
* Samples
	* Conditional Attributes
	* Conditional Elements
	* Elements list + Remove from within element
	* State-Verschachtelungen (z.B. div in div mit jeweils State) oder State-In-List
  * Heterogene Listen
* RawSpan
* All HTML API
* inputs
* Docu: Das geht so nicht - Trigger erklären!
        let! currentItems = Mutable.ofValue (ResizeArray())
        let addItem item = currentItems.value.Add(item)
        let removeItem item = currentItems.value.Remove(item) |> ignore
* Docu: Operators (+=, :=,. etc)        
* start: resolve issue "eventRemovalWorkaround with mutableLists example"
* Vide as no single case DU -> inlineIfLambda
* Module / NS restructuring
* Access HTMLElement from builder wirh let! or via map/iter
* SVG API
