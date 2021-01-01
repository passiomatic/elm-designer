import { Elm } from "./Main.elm"

// https://github.com/electron/electron/issues/2288#issuecomment-611231970
const isElectron = /electron/i.test(navigator.userAgent)

let remote = null
if (isElectron) {
  remote = window.require('electron').remote
}

function getIpc() {
  if (isElectron) {
    return window.require("electron").ipcRenderer
  } else {
    return FakeIpcRenderer
  }
}

const FakeIpcRenderer = {
  send: function (channel, arg) { },
  on: function (channel, arg) { }
}

const ipc = getIpc()

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

// Simple localStorage support 

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

// Copy to clipboard 

app.ports.copyToClipboard.subscribe(function (value) {
  if (!navigator.clipboard) {
    return
  }
  navigator.clipboard.writeText(value).then(function () {
  }, function (err) {
    console.error('Could not copy text to clipboard: ', err)
  })
});


// Select text input/textarea

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

// Set a drag image, see https://kryogenix.org/code/browser/custom-drag-image.html

app.ports.startDrag.subscribe(function (event) {
  var node = event.target.cloneNode(true);
  // Add a "template" class for nodes already in the page
  node.classList.add("template")
  node.title=""
  node.style.position = "absolute"
  node.style.top = "-999px"
  document.body.appendChild(node)
  event.dataTransfer.setDragImage(node, 0, 0)
});

// Set <head> links

app.ports.setFontLinks.subscribe(function (links) {
  let head = document.getElementsByTagName("head")[0]
  links.forEach((value) => {
    var el = document.createElement("link")
    el.setAttribute("href", value)
    el.setAttribute("rel", "stylesheet")
    head.appendChild(el)
  })
});

// Wire up menus

/* App and context menus */

function pageContextMenuTemplate(pageId) {
  return [
    {
      label: 'Delete Page',
      click: function (item, focusedWindow) {
        app.ports.onPageDelete.send(pageId)

      }
    },
    // {
    //   label: 'Duplicate Page',
    //   accelerator: 'CmdOrCtrl+D',
    //   click: function (item, focusedWindow) {
    //     app.ports.onPageDuplicate.send(pageId)
    //   }
    // }
  ]
}

app.ports.showPageContextMenu.subscribe(function (pageId) {
  showContextMenu(pageContextMenuTemplate(pageId))
});

function showContextMenu(menu) {
  const focusedWindow = remote.getCurrentWindow()

  if (!focusedWindow || focusedWindow === null) {
    return;
  }

  if(remote) {
    remote.Menu.buildFromTemplate(menu).popup({ window: focusedWindow })
  }
}

app.ports.setupAppMenu.subscribe(function (items) {
  ipc.send('setup-app-menu', items)
});


// Forward all app menu commands to Elm

window.onload = () => {
  ipc.on('command', (event, message, value) => {
    app.ports[`on${message}`].send(value)
  })
}

