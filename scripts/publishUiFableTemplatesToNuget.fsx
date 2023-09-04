#load "build.fsx"
open Build

cleanPackFolder ()

pack "../Vide.UI/Vide.UI.Fable/dotnetTemplates/templates.fsproj"

publish ()
