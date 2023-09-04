#load "build.fsx"
open Build

cleanPackFolder ()

pack "../Vide.Common/src/Vide.Common/Vide.Common.fsproj"
pack "../Vide.Common/src/Vide.Common/Vide.Common.UI.fsproj"
pack "../Vide.Common/src/Vide.Common/Vide.Common.Dsp.fsproj"

pack "../Vide.UI/Vide.UI.Fable/src/Vide.UI.Fable/Vide.UI.Fable.fsproj"

publish ()
