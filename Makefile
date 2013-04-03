.PHONY: all init

all: dist/setup-config
	cabal build

dist/setup-config: $(wildcard *.cabal) Makefile
	cabal configure --enable-tests
