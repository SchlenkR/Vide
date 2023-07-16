# Vide

Vide offers a unique approach for writing state-aware functions and composing them in a convenient and easy way. It is used for UI libraries targeting various platforms and technologies, streaming / DSP, workflow engines and perhaps more.

## Status

- Vide.Fable: It's usable, and the API is stable (although there are no battle-tested real-world apps donw with it). V1 release aimed for Q3 2023.
- Vide.Avalonia: Uses the same core implementation as Vide.Fable. The API still needs to get generated and tested.
- Vide.MAUI: see Vide.Avalonia

## Documentation

**Docs and Samples:** Learn more at the [Vide Website](https://vide-dev.io) or check out the [Use Cases](./Vide.Fable/src/DevApp/src/UseCases).

**Repo:** In case you want to contribute to the documentation, feel free to have a look at the [Vide Docu Repo](https://github.com/RonaldSchlenker/Vide.docs).

## Vide for Fable

See: [./Vide.Fable/README.md](./Vide.Fable/README.md)

## Build

See `./scripts` folder

## TODO

- document the directory structure of this repo!

## Release new NuGet Packages

What to do:

* Increment Versions + RelNotes:
	* Vide.Common (.fsproj)
	* Vide.Fable (.fsproj)
	* Vide.Fable dotnetTemplates (templates.fsproj)
* Update Package Ref Version
	* Vide.Fable dotnetTemplates (AppName.fsproj)
