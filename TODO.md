* Optimize "EvaluateView"
* Events: Immer auch den Context mitgeben
* let! ctx = context
* mutable lists
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
	* State-Verschachtelungen (z.B. div in div mit jeweils State)
