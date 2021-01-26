module.exports = function (app, shell, isMac, insertMenuItems) {
  const template = [{
      label: 'Edit',
      submenu: [{
          label: 'Undo',
          accelerator: 'CmdOrCtrl+Z',
          click: function (item, focusedWindow) {
            if (focusedWindow)
              focusedWindow.webContents.send("command", "UnDo", null)
          }
        },
        {
          label: 'Redo',
          accelerator: 'Shift+CmdOrCtrl+Z',
          click: function (item, focusedWindow) {
            if (focusedWindow)
              focusedWindow.webContents.send("command", "ReDo", null)
          }
        },
        {
          type: 'separator'
        },
        {
          label: 'Cut',
          accelerator: 'CmdOrCtrl+X',
          role: 'cut'
        },
        {
          label: 'Copy',
          accelerator: 'CmdOrCtrl+C',
          role: 'copy'
        },
        {
          label: 'Paste',
          accelerator: 'CmdOrCtrl+V',
          role: 'paste'
        },
        {
          label: 'Select All',
          accelerator: 'CmdOrCtrl+A',
          role: 'selectall'
        }
      ]
    },
    {
      label: 'Insert',
      //id: "INSERT",
      submenu: [{
        label: 'New Page',
        accelerator: 'CmdOrCtrl+Shift+Alt+N',
        click: function (item, focusedWindow) {
          if (focusedWindow)
            focusedWindow.webContents.send("command", "PageAdd", null)
        }
      }].concat(insertMenuItems)
    },
    {
      label: 'View',
      submenu: [{
          label: 'Reload',
          accelerator: 'CmdOrCtrl+R',
          click: function (item, focusedWindow) {
            if (focusedWindow)
              focusedWindow.reload();
          }
        },
        {
          label: 'Toggle Full Screen',
          accelerator: (function () {
            if (isMac)
              return 'Ctrl+Command+F';
            else
              return 'F11';
          })(),
          click: function (item, focusedWindow) {
            if (focusedWindow)
              focusedWindow.setFullScreen(!focusedWindow.isFullScreen());
          }
        },
        {
          label: 'Toggle Developer Tools',
          accelerator: (function () {
            if (isMac)
              return 'Alt+Command+I';
            else
              return 'Ctrl+Shift+I';
          })(),
          click: function (item, focusedWindow) {
            if (focusedWindow)
              focusedWindow.toggleDevTools();
          }
        },
      ]
    },
    {
      label: 'Window',
      role: 'window',
      submenu: [{
          label: 'Minimize',
          accelerator: 'CmdOrCtrl+M',
          role: 'minimize'
        },
        {
          label: 'Close',
          accelerator: 'CmdOrCtrl+W',
          role: 'close'
        },
      ]
    },
    {
      label: 'Help',
      role: 'help',
      submenu: [{
        label: 'Learn More',
        click: function () {
          shell.openExternal('http://lab.passiomatic.com/elm-designer')
        }
      }, ]
    },
  ];

  if (isMac) {
    const {
      name
    } = app;
    template.unshift({
      label: name,
      submenu: [{
          label: 'About ' + name,
          role: 'about'
        },
        {
          type: 'separator'
        },
        {
          label: 'Services',
          role: 'services',
          submenu: []
        },
        {
          type: 'separator'
        },
        {
          label: 'Hide ' + name,
          accelerator: 'Command+H',
          role: 'hide'
        },
        {
          label: 'Hide Others',
          accelerator: 'Command+Shift+H',
          role: 'hideothers'
        },
        {
          label: 'Show All',
          role: 'unhide'
        },
        {
          type: 'separator'
        },
        {
          label: 'Quit',
          accelerator: 'Command+Q',
          click: function () {
            app.quit();
          }
        },
      ]
    });
    const windowMenu = template.find(function (m) {
      return m.role === 'window'
    })
    if (windowMenu) {
      windowMenu.submenu.push({
        type: 'separator'
      }, {
        label: 'Bring All to Front',
        role: 'front'
      });
    }
  }
  return template;
}