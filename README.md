# Elm Designer—A code generator for Elm UI

![Elm Designer interface](./assets/screenshot.jpg)

## Current status

The application is in early stages of development and supports a subset of Elm UI.

Version 0.1 goals are:

- Create and transform simple designs into Elm code
- Auto-save designs into browser `localStorage`
- Cover Elm UI `row`, `column`, `textColumn` layout primitives and most of the form widgets; support padding and spacing; allow fonts formatting with native and external web fonts (thanks to Google Fonts)  

## Download 

Elm Designer is distributed as an Electron app. Right now there’s a [macOS binary available][d] to download and you can run Elm Designer sources on Windows and Linux systems via CLI.

## Known issues

- Color picker is quite limited at the moment since Elm Designer is using HTML 5 `input type=color`. Specifically you can't reset a color or specify `inherit`. See [#1][issue1]
- When switching fonts the weight of the new font should match, or be closer as possible, to the old one. See [#2][issue2]
- ~~Non-fluid pages larger than current workspace dimensions push right panel out of app window~~

## Build Elm Designer from sources

If you need to edit the source files the `makefile` on the repository root contains a bunch on tasks which automate the most common operations.

### Install Parcel

Elm Designer uses [Parcel][2] to compile Elm and SASS source files. Please read up [these instructions][3] to install Parcel 1.12.4 before building Elm Designer from sources.

### Run with Electron

You can run [Electron][4] as a command-line app and then point it to the `main.js` entry point.

So, first install Electron and all its dependencies locally:

    npm ci

Then run:

    make run

This will build the app assets with Parcel in production mode, copy the files into the `electron-app` folder in the repo, and finally run Electron itself.

[1]: https://github.com/electron/electron-packager
[2]: https://parceljs.org
[3]: https://parceljs.org/getting_started.html
[4]: https://www.electronjs.org
[issue1]: https://github.com/passiomatic/elm-designer/issues/1 
[issue2]: https://github.com/passiomatic/elm-designer/issues/2 
[d]: https://github.com/passiomatic/elm-designer/releases/tag/v0.1.0