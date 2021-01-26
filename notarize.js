const fs = require('fs');
const path = require('path');
var electronNotarize = require('electron-notarize');

// Same appId in electron-builder
const appId = 'com.passiomatic.ElmDesigner'

module.exports = async function (params) {
    // Only notarize the app on Mac OS only.
    if (params.electronPlatformName !== 'darwin') {
        return;
    }
    
    let appPath = path.join(params.appOutDir, `${params.packager.appInfo.productFilename}.app`);
    if (!fs.existsSync(appPath)) {
        throw new Error(`Cannot find application at: ${appPath}`);
    }

    console.log(`Notarizing ${appId} found at ${appPath}`);

    try {
        await electronNotarize.notarize({
            appBundleId: appId,
            appPath: appPath,
            appleId: process.env.appleId,
            appleIdPassword: process.env.appleIdPassword,
        });
    } catch (error) {
        console.error(error);
    }

    console.log(`Done notarizing ${appId}`);
};