# Vide

Vide is a declarative view library written in F#. It implements a core that can be used in various application frameworks. It currently uses Fable (❤).

> Vide is in an early prototype state, and it may be that many things you read here will already be obsolete!

The prototype implements the core, a Fable API, and a Fable app that demos several use cases.

The basic idea (composing state-aware functions) is described in the [contribution to the 2019s 'Applied F# Challenge'](https://github.com/RonaldSchlenker/applied_fsharp_challenge/blob/master/docs/index.md).

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

## Samples

**All samples shown here can be executed in the [branch demos/public01](https://github.com/RonaldSchlenker/Vide/tree/demos/public01)**

Given the `index.html` with an "app" container and a script tag for app kickoff:

```html
<body>
  <div id="app"></div>
  <script type="module" src="./build/app.js"></script>
</body>
```

In `app.fs`, you will find variouse samples, including the famous "Counter"-Sample, known from various frameworks:

```fsharp
let videApp =
    vide {
        let! count = Mutable.ofValue 0

        div {
            $"Count = {count.Value}"
        }

        button .onclick(fun _ -> count -= 1) { "dec" }
        button .onclick(fun _ -> count += 1) { "inc" }
    }
```

Also in `app.fs`, start the app by hosting it in the app-div:

```fsharp
let appHolder = document.getElementById("app")
let videMachine = Vide.Fable.startApp appHolder videApp (fun sender state -> ())
videMachine.Eval()
```

![Counter Sample](./docu/videcounter.gif)
