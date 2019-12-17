# Mosaiku App

This is a test project to try [Fable](https://fable.io/) with [Paket](https://fsprojects.github.io/Paket/) and [FAKE](https://fake.build/) from scratch.

## What I've done

* Add Paket to my projet: https://fsprojects.github.io/Paket/getting-started.html.
  .gitignore for paket:
    paket-files
    packages/
    .paket/obj
* Create an empty project: https://fable.io/docs/your-fable-project/project-file.html.
   .gitignore for f#:
    src/bin
    src/obj
* Add FAKE to my project: http://fake.build/fake-gettingstarted.html.
* Copy the source code from the Fable minimal project: https://github.com/fable-compiler/fable2-samples/tree/master/minimal.

## Building and running the app (from the minimal README)

* Install JS dependencies: `npm install`
* Start Webpack dev server: `npx webpack-dev-server` or `npm start`
* After the first compilation is finished, in your browser open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

## Building and running and debugging the app

* Start Webpack dev server with: `npm start --inspect`
