#load "build.fsx"
open Build

cleanPackFolder ()
pack "../Vide.Common/src/Vide.Common/Vide.Common.fsproj"
pack "../Vide.Fable/src/Vide.Fable/Vide.Fable.fsproj"
pack "../Vide.Fable/dotnetNewTemplates/templates.fsproj"
publish ()
