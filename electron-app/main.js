'use strict'
const { screen, app, ipcMain, BrowserWindow, Menu, dialog, shell } = require('electron')
const appMenu = require('./menus');

let mainWindow

app.on('ready', createWindow)

function createWindow() {
  const { width, height } = screen.getPrimaryDisplay().workAreaSize

  mainWindow = new BrowserWindow({
    width: width,
    height: height,
    //center: true,
    webPreferences: {
      nodeIntegration: true
    }
  })

  mainWindow.loadURL(`file://${__dirname}/app.html`)

  // Application menu
  const menu = appMenu(app, shell);
  Menu.setApplicationMenu(Menu.buildFromTemplate(menu));

  // Open dev tools by default so we can see any console errors
  //mainWindow.webContents.openDevTools()

  mainWindow.on('closed', function () {
    mainWindow = null
  })

}

/* Context menus */

function createPageContextMenu(pageId) {
  return [
    {
      label: 'Delete Page',
      click: function (item, focusedWindow) {
        mainWindow.webContents.send("command", "PageDelete", pageId)
      }
    },
    // {
    //   label: 'Duplicate Page',
    //   accelerator: 'CmdOrCtrl+D',
    //   click: function (item, focusedWindow) {
    //     mainWindow.webContents.send("command", "DuplicatePage", pageId)
    //   }
    // }
  ]
}

ipcMain.on('show-page-context-menu', (event, pageId) => {
  //console.log(pageId)
  showContextMenu(createPageContextMenu(pageId))
})

function showContextMenu(menu) {
  const focusedWindow = BrowserWindow.getFocusedWindow();

  if (!focusedWindow || focusedWindow === null) {
    return;
  }

  Menu.buildFromTemplate(menu).popup({ window: focusedWindow });

}

/* Mac specific things */

// When you close all the windows on a non-mac OS it quits the app
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

// Ff there is no mainWindow it creates one (like when you click the dock icon)
app.on('activate', () => {
  if (mainWindow === null) { createWindow() }
})