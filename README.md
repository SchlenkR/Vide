# Vide

Vide offers a unique approach for writing state-aware functions and composing them in a convenient and easy way. It is used for UI libraries targeting various platforms and technologies, streaming / DSP, workflow engines and perhaps more.

> Vide is in an early prototype state, and it may be that many things you read here will already be obsolete!

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
