all: dev

constants: 
	npm run constants
	
build: clean constants 
	npm run build

serve: build
	npx serve dist

build-no-maps: clean constants
	npm run build-no-maps

build-optimize-2: build
	elm-optimize-level-2 src/Main.elm --output dist/app-optimized.js

dev-no-debug: constants
	npm run dev-no-debug

dev: constants
	npm run dev

landing: constants
	npm run landing

run: 
	python3 server/manage.py runserver

clean: 
	npm run clean