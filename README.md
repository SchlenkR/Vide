
*Support my Work*

Buy a **PXL Clock** and help me create more videos like this!
Use code **RONALD** for a **25€ discount**:

[https://www.pxlclock.com/?ref=RONALD](https://www.pxlclock.com/?ref=RONALD)

PXL Clock is a fun device, made with ❤️ - and it's programmable in an easy and quick way.

<p align="center">
  <a href="https://www.pxlclock.com/?ref=RONALD">
    <img width="842" height="832" alt="468354531-9b92c9d7-b20b-4316-8104-ac980fa449d5" src="https://github.com/user-attachments/assets/0a5a495d-731b-4f65-ac8f-3719f9b9010a" />
  </a>
</p>

Find out more:

- On the [PXL Clock Discord Server](https://discord.gg/KDbVdKQh5j)
- check out the [PXL Clock Repo on GitHub](https://github.com/CuminAndPotato/PXL-Clock)
- Visit the official [PXL Clock Store](https://www.pxlclock.com/?ref=RONALD)

<p align="center">
  <h3>Join the PXL Clock Community on Discord</h3>
  <a href="https://discord.gg/KDbVdKQh5j">
    <img src="https://img.shields.io/badge/Discord-Join%20Server-blue?style=flat-square&logo=discord" alt="Join Our Discord">
  </a>
</p>

---

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
