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

  var node = payload.event.target.cloneNode(true);
  
  node.title = "";
  node.style.position = "absolute";
  node.style.top = "-9999px";
  if (payload.width > 0 && payload.height > 0) {
    node.style.width = payload.width + "px";
    node.style.height = payload.height + "px";
  } else {
    // Don't add a ghost class for pages already in the workspace
    node.classList.add("library__item-ghost");
  }
  document.body.appendChild(node);

  var clientRect = payload.event.target.getBoundingClientRect();
  // console.log("target.clientRect.top "+ clientRect.top)
  // console.log("target.clientRect.left "+ clientRect.left)
  // console.log("event.clientX "+ payload.event.clientX)
  // console.log("event.clientY "+ payload.event.clientY)
  var offsetX = payload.event.clientX - clientRect.left;
  var offsetY = payload.event.clientY - clientRect.top;
  payload.event.dataTransfer.setDragImage(node, offsetX, offsetY);
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
