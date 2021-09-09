all: dev

build:
	parcel build src/index.html --public-url ./ --out-file index.html

build-no-maps:
	parcel build src/index.html --no-source-maps --public-url ./ --out-file index.html

dev-no-debug:
	NODE_ENV=production parcel src/index.html

dev:
	parcel src/index.html

clean: 
	rm -fR dist
	rm -fR build