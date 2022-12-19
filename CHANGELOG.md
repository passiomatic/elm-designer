# Version 0.4.2 • 2022-06-14

## New features and tweaks

* Import and export current document as JSON file
* Display image width, height and format in 'Info' section 

## Bugs fixed 

* Fix labelHidden emit
* Made image resizing more predictable when tweaking its width and height settings

---

# Version 0.4.1 • 2022-05-04

## New features and tweaks

* Implement collapsible inspector subpanels
* Allow to tweak pages vertical spacing
* Add images 'Info' section on inspector

## Bugs fixed 

* Remove up/down arrows in Firefox
* Fix logic to select custom preset sizes

---

# Version 0.4.0 • 2022-02-28

Elm Designer is now a web app. No more Electron binaries to download. Yay!

## New features and tweaks

* New multi-page, scrollable workspace
* Reworked outline view and context menu
* Add label color for text fields
* Add inner/outer shadow type toggler (thanks @axelbdt!)

## Bugs fixed 

* Image doesn't collapse anymore when inserted into a row
* "Insert" menu now allows only valid parent-child combinations

--- 

# Version 0.3.0 • 2021-07-11

⚠️ **Document format is changed from previous version so you won't be able to load your existing designs.**

## New features and tweaks

* Added undo/redo menu commands for destructive operations (thanks @CharlonTank!)
* Added support for relative positioned children (above, in front, etc.)
* Added support for element shadow
* Added UI to specify form fields label position: above, below, left, right and hidden
* Added UI for border style: solid, dashed and dotted

## Bugs fixed 

* Node offset and rotation values are not generated 
* Can't remove Label on text fields

---

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
