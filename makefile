all: dev

build: clean
	npm run build

build-no-maps: clean
	npm run build-no-maps

dev-no-debug:
	npm run dev-no-debug

dev:
	npm run dev

clean: 
	npm run clean