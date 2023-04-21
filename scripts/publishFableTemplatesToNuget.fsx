#load "build.fsx"
open Build

cleanPackFolder ()
pack "../Vide.Fable/dotnetTemplates/templates.fsproj"
publish ()
