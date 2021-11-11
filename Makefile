default: format
	dune build
	python replacer.py

format:
	dune build @fmt --auto-promote

release: format
	dune build --profile release
	python replacer.py

clean:
	dune clean

test: default
	dune runtest

install: release
	dune install

uninstall: release
	dune uninstall

.PHONY: default format release clean test test-validate install uninstall
