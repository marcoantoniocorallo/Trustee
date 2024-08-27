all: build

.SILENT: build test clean

build:
	dune build
	mkdir -p plugin
	cp -fr test/plugin/* ./plugin
	cp -f _build/default/bin/main.exe ./TFhree
	echo "Complete. The interpreter is: TFhree"

install: build
	mkdir -p _install
	cp -f ./LICENSE _install/LICENSE
	cp -f ./README.md _install/README.md
	cp -f _build/default/bin/main.exe _install/main.exe

setup:
	bash setup.sh
	echo "Complete."

test:
	dune runtest
	echo "Complete."

clear: clean

clean:
	rm -fr _build
	rm -fr _install
	rm -f ./TFhree
	rm -fr plugin
	echo "Complete."

.PHONY: all build install clear clean test setup