#load "build.fsx"
open Build

cleanPackFolder()
pack "../Vide.Fable/dotnetNewTemplates/templates.fsproj"
publish ()
