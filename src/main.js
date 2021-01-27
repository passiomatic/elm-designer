'use strict'
const { screen, app, ipcMain, BrowserWindow, Menu, dialog, shell, fs } = require('electron')
const appMenu = require('./menus');

const isMac = process.platform === 'darwin'

let mainWindow

app.on('ready', createWindow)

function createWindow() {
  const { width, height } = screen.getPrimaryDisplay().workAreaSize

  mainWindow = new BrowserWindow({
    width: width,
    height: height,
    webPreferences: {
      nodeIntegration: true,
      enableRemoteModule: true
    }
  })

  mainWindow.loadURL(`file://${__dirname}/app.html`)

  // Application menu
  // const menu = appMenu(app, shell, isMac, [])
  // Menu.setApplicationMenu(Menu.buildFromTemplate(menu))

  // Open dev tools by default so we can see any console errors
  //mainWindow.webContents.openDevTools()

  mainWindow.on('closed', function () {
    mainWindow = null
  })

}

/* Menu callbacks */ 

const imageFiles = [
  { name: 'Images', extensions: ['jpg', 'png', 'gif', 'svg'] }
]

function addImage(focusedWindow) {
  const fileNames = dialog.showOpenDialogSync(focusedWindow,
      { filters: imageFiles,        
        properties: ["openFile", "multiSelections"],
        //message : "Select one or more images"
      })
  if(fileNames===undefined) {
    // User pressed cancel
    return
  }
  focusedWindow.webContents.send("renderer", "InsertImage", fileNames)
}

/* Setup app menu with passed 'Insert' menu items */

ipcMain.on('setup-app-menu', (event, insertItems) => {
  var prevGroup = ""
  var menuItems = insertItems.reduce((menu, item) => {
    if (item.group != prevGroup) {
      menu.push({
        type: 'separator'
      })
      prevGroup = item.group
    }
    // Populate callbacks
    item.click = function (item, focusedWindow) {
      mainWindow.webContents.send("command", "InsertNode", item.label)
    }
    menu.push(item)
    return menu
  }, [])
  const menu = appMenu(app, shell, isMac, menuItems, addImage);
  Menu.setApplicationMenu(Menu.buildFromTemplate(menu));
})

/* Mac specific things */

// When you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (isMac) { app.quit() }
})

// If there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})