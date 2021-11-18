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

run: 
	python3 server/manage.py runserver

clean: 
	npm run clean