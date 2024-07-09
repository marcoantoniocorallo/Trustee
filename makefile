all: build

build:
	dune build
	mkdir plugin -p
	cp -fr test/plugin/* ./plugin
	cp -f _build/default/bin/main.exe ./TFhree

install: build 
	mkdir -p _install
	cp -f ./LICENSE _install/LICENSE
	cp -f ./README.md _install/README.md
	cp -f _build/default/bin/main.exe _install/main.exe

clear: clean

clean:
	rm -fr _build
	rm -fr _install
	rm -f ./TFhree
	rm -fr plugin

.PHONY: all build install clear clean