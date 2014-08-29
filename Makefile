.PHONY: sandbox-install install clean

CORES = 1

sandbox-install:
	cabal sandbox init
	cabal install -j$(CORES) --bindir=dist/bin

install:
	cabal sandbox init
	cabal install -j$(CORES) --bindir=$(HOME)/.cabal/bin

clean:
	rm -rf cabal.sandbox.config .cabal-sandbox dist
