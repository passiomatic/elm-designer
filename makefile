all: dev

build:
	parcel build src/index.html --no-source-maps --target electron -d electron-app --public-url ./ --out-file app.html

dev-no-debug:
	NODE_ENV=production parcel src/index.html

dev:
	parcel src/index.html

clean: 
	rm -f electron-app/app.html
	rm -f electron-app/*.css
	rm -f electron-app/src.*.js
	rm -fR electron.iconset
	rm -fR dist
	rm -fR build

run: build
	electron electron-app/main.js

package-mac: clean build icon
	electron-builder -m

icon: 
	mkdir electron.iconset
	sips -z 16 16 icon.png --out electron.iconset/icon_16x16.png
	sips -z 32 32 icon.png --out electron.iconset/icon_16x16@2x.png
	sips -z 32 32 icon.png --out electron.iconset/icon_32x32.png
	sips -z 64 64 icon.png --out electron.iconset/icon_32x32@2x.png
	sips -z 128 128 icon.png --out electron.iconset/icon_128x128.png
	sips -z 256 256 icon.png --out electron.iconset/icon_128x128@2x.png
	sips -z 256 256 icon.png --out electron.iconset/icon_256x256.png
	sips -z 512 512 icon.png --out electron.iconset/icon_256x256@2x.png
	sips -z 512 512 icon.png --out electron.iconset/icon_512x512.png
	cp icon.png electron.iconset/icon_512x512@2x.png
	iconutil -c icns --output electron.iconset/app.icns electron.iconset