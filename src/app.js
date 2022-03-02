import { Elm } from "./Main.elm";

var w = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
var h = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

var seeds = new Uint32Array(4);
window.crypto.getRandomValues(seeds);

var app = Elm.Main.init({
  flags: {
    width: w,
    height: h,
    seed1: seeds[0],
    seed2: seeds[1],
    seed3: seeds[2],
    seed4: seeds[3],
    platform: navigator.platform,
  },
  node: document.getElementById("app"),
});

// -------------------------------
// Simple localStorage support
// -------------------------------

var storageKey = "lastDocument";

app.ports.saveDocument.subscribe(function (value) {
  localStorage.setItem(storageKey, value);
});

app.ports.loadDocument.subscribe(function () {
  let value = localStorage.getItem(storageKey);
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
    return;
  }
  navigator.clipboard.writeText(value).then(
    function () {},
    function (err) {
      console.error("Could not copy text to clipboard: ", err);
    }
  );
});

// -------------------------------
// Select text input/textarea
// -------------------------------

app.ports.selectText.subscribe(function (id) {
  // We need to wait for Elm to render the <textarea>
  //   so we can find it in the DOM
  window.requestAnimationFrame((timespamp) => {
    let el = document.getElementById(id);
    if (el) {
      el.select();
    }
  });
});

// Set a drag image, see:
//
// * https://kryogenix.org/code/browser/custom-drag-image.html
// * https://transitory.technology/set-drag-image/
//
app.ports.setDragImage.subscribe(function (payload) {
  // FF compatibility
  payload.event.dataTransfer.setData("text", "");
  var clientRect = payload.event.target.getBoundingClientRect();

  var node = null;
  if (payload.dragging) {
    // Dragging elements on workspace
    node = payload.event.target.cloneNode(false);
    // Safari has issues with big ghost drag images, so set explictly final dimensions
    node.style.width = clientRect.width + "px";
    node.style.height = clientRect.height + "px";
    node.classList.add("ghost-image");
  } else {
    // Dragging from Library/outline
    node = payload.event.target.cloneNode(true);
    node.classList.add("ghost-image");
    node.classList.add("library__item-ghost");
  }
  node.title = "";
  node.style.position = "absolute";
  node.style.right = "9999px"; // Put offscreen
  document.body.appendChild(node);

  // console.log("event.clientX "+ payload.event.clientX)
  // console.log("event.clientY "+ payload.event.clientY)
  var offsetX = payload.event.clientX - clientRect.left;
  var offsetY = payload.event.clientY - clientRect.top;
  payload.event.dataTransfer.setDragImage(node, offsetX, offsetY);
});

app.ports.endDrag.subscribe(function () {
  // Cleanup
  document.querySelectorAll(".ghost-image").forEach((el) => {
    el.remove(); 
  });
});

// app.ports.setDragCursor.subscribe(function (event, cursor) {
//   event.dataTransfer.dropEffect = cursor
// });

// -------------------------------
// Set <head> links
// -------------------------------

app.ports.setFontLinks.subscribe(function (links) {
  let head = document.getElementsByTagName("head")[0];
  links.forEach((value) => {
    var el = document.createElement("link");
    el.setAttribute("href", value);
    el.setAttribute("rel", "stylesheet");
    head.appendChild(el);
  });
});

// -------------------------------
// Notifications
// -------------------------------

app.ports.showNotification.subscribe(function (options) {
  if (!("Notification" in window)) {
    console.warn("This browser does not support desktop notification");
  } else if (Notification.permission === "granted") {
    var notification = new Notification(options.title, {
      body: options.message,
    });
  }
  // Never asked
  else if (Notification.permission !== "denied") {
    Notification.requestPermission().then(function (permission) {
      if (permission === "granted") {
        var notification = new Notification(options.title, {
          body: options.message,
        });
      }
    });
  }
});
