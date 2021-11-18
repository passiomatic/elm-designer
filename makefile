all: dev

constants: 
	npm run constants
	
build: clean constants 
	npm run build

build-no-maps: clean constants
	npm run build-no-maps

dev-no-debug:
	npm run dev-no-debug

dev:
	npm run dev

clean: 
	npm run clean