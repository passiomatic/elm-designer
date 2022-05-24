all: dev

constants: 
	npm run constants
	
build: clean constants 
	npm run build

build-no-maps: clean constants
	npm run build-no-maps

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