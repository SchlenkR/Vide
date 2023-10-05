#load "build.fsx"
open Build

cleanPackFolder ()

pack "../Vide.Common/src/Vide.Common/Vide.Common.fsproj"
pack "../Vide.Common/src/Vide.Common.UI/Vide.Common.UI.fsproj"
pack "../Vide.Common/src/Vide.Common.DSP/Vide.Common.DSP.fsproj"

pack "../Vide.UI/Vide.UI.Fable/src/Vide.UI.Fable/Vide.UI.Fable.fsproj"
pack "../Vide.UI/Vide.UI.Avalonia/src/Vide.UI.Avalonia/Vide.UI.Avalonia.fsproj"
pack "../Vide.UI/Vide.UI.Avalonia/src/Vide.UI.Avalonia.Interactive/Vide.UI.Avalonia.Interactive.fsproj"

publish ()
