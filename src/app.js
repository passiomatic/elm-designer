import { Elm } from "./Main.elm"

var w = Math.max(
  document.documentElement.clientWidth,
  window.innerWidth || 0
);
var h = Math.max(
  document.documentElement.clientHeight,
  window.innerHeight || 0
);

var seeds = new Uint32Array(4)
window.crypto.getRandomValues(seeds)

var app = Elm.Main.init({
  flags: {
    width: w,
    height: h,
    seed1: seeds[0],
    seed2: seeds[1],
    seed3: seeds[2],
    seed4: seeds[3],
  },
  node: document.getElementById("app"),
});

// -------------------------------
// Simple localStorage support 
// -------------------------------

var storageKey = "lastDocument"

app.ports.saveDocument.subscribe(function (value) {
  localStorage.setItem(storageKey, value)
});

app.ports.loadDocument.subscribe(function () {
  let value = localStorage.getItem(storageKey)
  // Sanity check for first run
  if (value) {
    app.ports.onDocumentLoad.send(value);
  }
});

// -------------------------------
// Copy to clipboard 
// -------------------------------

app.ports.copyToClipboard.subscribe(function (value) {
  if (!navigator.clipboard) {
    return
  }
  navigator.clipboard.writeText(value).then(function () {
  }, function (err) {
    console.error('Could not copy text to clipboard: ', err)
  })
});

// -------------------------------
// Select text input/textarea
// -------------------------------

app.ports.selectText.subscribe(function (id) {
  // We need to wait for Elm to render the <textarea> 
  //   so we can find it in the DOM
  window.requestAnimationFrame((timespamp) => {
    let el = document.getElementById(id)
    if (el) {
      el.select()
    }
  }
  );
});

// Set a drag image, see:
//
// * https://kryogenix.org/code/browser/custom-drag-image.html
// * https://transitory.technology/set-drag-image/
//
app.ports.setDragImage.subscribe(function (event) {  
  // FF compat.
  event.dataTransfer.setData('text', '');

  var node = event.target.cloneNode(true);

  // Add a "template" class for nodes already in the page
  node.classList.add("template")
  node.title=""
  node.style.position = "absolute"
  node.style.top = "-9999px"  
  document.body.appendChild(node)

  var clientRect = event.target.getBoundingClientRect()
  var offsetX = event.clientX - clientRect.left
  var offsetY = event.clientY - clientRect.top
  event.dataTransfer.setDragImage(node, offsetX, offsetY)
});

// app.ports.setDragCursor.subscribe(function (event, cursor) {
//   event.dataTransfer.dropEffect = cursor
// });

// -------------------------------
// Set <head> links
// -------------------------------

app.ports.setFontLinks.subscribe(function (links) {
  let head = document.getElementsByTagName("head")[0]
  links.forEach((value) => {
    var el = document.createElement("link")
    el.setAttribute("href", value)
    el.setAttribute("rel", "stylesheet")
    head.appendChild(el)
  })
});

// -------------------------------
// Notifications
// -------------------------------

app.ports.showNotification.subscribe(function (options) {
  const notification = new Notification(options.title, {
    body: options.message
  })  
});

app.ports.showMessageBox.subscribe(function (options) {
  console.error(options.message)
});
