# Vide

Vide is a declarative view library written in F#. It implements a core that can be used in various application frameworks. It currently uses Fable (❤).

> Vide is in an early prototype state.

The prototype implements the core, a Fable API, and a Fable app that demos several use cases.

The basic idea (composing stateful functions) is described in my [contribution to the 2019s 'Applied F# Challenge'](https://github.com/RonaldSchlenker/applied_fsharp_challenge/blob/master/docs/index.md).

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 5.0 or higher
* [node.js](https://nodejs.org)
* An F# editor like Visual Studio, Visual Studio Code with [Ionide](http://ionide.io/) or [JetBrains Rider](https://www.jetbrains.com/rider/)

## Building and running the app

* Install dependencies: `npm install`
* Start the compiler in watch mode and a development server: `npm start`
* After the first compilation is finished, in your browser open: http://localhost:3000/

Any modification you do to the F# code will be reflected in the web page after saving.

> Note: check the "scripts" section in `package.json` to see the commands triggered by the steps above.
