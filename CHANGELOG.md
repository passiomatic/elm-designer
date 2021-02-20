# Version 0.2.1 • 2021-02-20

## New features and tweaks

* Show elements textual content in tree item labels
* Added Inter font

## Bugs fixed 

* Honor height setting on form library elements
* While on a inspector field hitting backspace do not delete the selected element anymore

**Note**: document format is the same of the previous version so your designs will continue to work after the upgrade.

---

# Version 0.2.0 • 2021-02-06

⚠️ **Document format is changed from previous version so you won't be able to load your existing designs.**

## New features and tweaks

* Insert images into page via drag and drop
* Added alignment button to toggle centering on/off
* Added inspector settings to specify exact width and height pixel values
* Added letter and word spacing styles to inspector
* Drop library elements directly on the page or on outline pane
* Use Electron API to show error messages
* Added Source Sans Pro, Source Serif Pro, Source Mono Pro, Roboto Mono and Space Mono fonts
* Show inherited font family and weight styles in gray and italic instead of showing them between parens
* Mark the 'fold' in fixed height viewports

## Bugs fixed 

* Better handling of font weight adjust while changing family  
* Made the inline text editor to honor node text alignment
* Allow selection of left/right aligned (floated) elements within their container

---

# Version 0.1.1 • 2021-01-09

## New features and tweaks

* Added an "Insert" menu. This makes more obvious how to add a new element to the page 
* Use up and down arrows to increment/decrement all the numeric fields in the inspector pane
* Added element move right/left and move up/down fields to  inspector pane. These match Elm UI's `Element.move*` functions
* Grouped fonts in the font family dropdown
* Added IBM Plex Sans, Alegreya Sans, Lora and Libre Baskerville Google Fonts
 
## Bugs fixed 

* Workspace area now scrolls horizontally when designed page is bigger than app window
* Wired up background image sizing controls
* Codegen: emit border corners values (thank you @CharlonTank) 
* Codegen: native font-stack is now specified correctly 

**Note**: document format is the same of the previous version so your designs will continue to work after the upgrade.

---

# Version 0.1.0 • 2020-12-27

First public version.
