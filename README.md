# Vide

Vide offers a unique approach for writing state-aware functions and composing them in a convenient and easy way. It is used for UI libraries targeting various platforms and technologies, streaming / DSP, workflow engines and perhaps more.

<h1>
  Vide is not actively maintained at the moment in regards of 
  <ul>
    <li>new features</li>
    <li>bug-fixes</li>
    <li>security updates</li>
    <li>keeping up with runtimes and dependencies</li>
  </ul>
</h1>

## Status

- Vide.UI.Fable: It's usable, and the API is stable (although there are no battle-tested real-world apps donw with it). V1 release aimed for Q3 2023.
- Vide.UI.Avalonia: Uses the same core implementation as Vide.UI.Fable. The API still needs to get generated and tested.
- Vide.UI.Maui: see Vide.UI.Avalonia

## Documentation

**Docs and Samples:** Learn more at the [Vide Website](https://vide-dev.io) or check out the [Use Cases](./Vide.UI.Fable/src/DevApp/src/UseCases).

**Repo:** In case you want to contribute to the documentation, feel free to have a look at the [Vide Docu Repo](https://github.com/RonaldSchlenker/Vide.docs).

## Vide for Fable

See: [./Vide.UI.Fable/README.md](./Vide.UI.Fable/README.md)

## Build

See `./scripts` folder

## TODO

- document the directory structure of this repo!

## Release new NuGet Packages

What to do:

* Increment Versions + RelNotes:
	* Vide.Common (.fsproj)
	* Vide.UI.Fable (.fsproj)
	* Vide.UI.Fable dotnetTemplates (templates.fsproj)
* Update Package Ref Version
	* Vide.UI.Fable dotnetTemplates (TemplateAppName.fsproj)
* Fablke
  * Check if all files are included correctly
  * Check the correct versions are referenced for Vide.UI.Fable in the template
  * npm i for the tempalte and update lockfile
  * Are the correct SDK versions referenced in the template
  * instanciate the template; npm i; npm run build