all: build

.SILENT: build test clean setup

build:
	dune build
	mkdir -p plugin
	cp -fr test/plugin/* ./plugin
	cp -f _build/default/bin/main.exe ./Trustee
	echo "Complete. The interpreter is: Trustee"

install: build
	mkdir -p _install
	cp -f ./LICENSE _install/LICENSE
	cp -f ./README.md _install/README.md
	cp -f _build/default/bin/main.exe _install/main.exe

setup:
	bash setup.sh

test:
	dune runtest
	echo "Test completed."

clear: clean

clean:
	rm -fr _build
	rm -fr _install
	rm -f ./Trustee
	rm -fr plugin
	echo "Clean."

.PHONY: all build install clear clean test setup