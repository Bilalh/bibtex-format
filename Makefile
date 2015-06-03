.PHONY: sandbox-install install clean

CORES = 1


install:
	cabal sandbox init
	cabal install -j$(CORES) --bindir=$(HOME)/.cabal/bin

sandbox-install:
	cabal sandbox init
	cabal install -j$(CORES) --bindir=dist/bin

clean:
	rm -rf cabal.sandbox.config .cabal-sandbox dist

# load ghci with all .hs files if `files` is not specifed
# `args` can be used to specify extra args to ghci
# Usage:
# `make ghci` to run ghci on .hs files
# `make ghci files=<files>` to run on specific files
ghci:
	@cabal exec ghci -- -isrc  \
		-XOverloadedStrings                        \
		-XScopedTypeVariables                      \
		-XTemplateHaskell                          \
		-XQuasiQuotes                              \
		-XLambdaCase                               \
		-XRecordWildCards                          \
		-XNamedFieldPuns                           \
		-XMultiWayIf                               \
		-XFlexibleContexts                         \
		-XFlexibleInstances                        \
		-fwarn-incomplete-patterns                 \
		-fwarn-incomplete-uni-patterns             \
		-fwarn-missing-signatures                  \
		-fwarn-name-shadowing                      \
		-fwarn-orphans                             \
		-fwarn-overlapping-patterns                \
		-fwarn-tabs                                \
		-fwarn-unused-do-bind                      \
		-fwarn-unused-matches                      \
		-Wall                                      \
		-fghci-hist-size=500                       \
		${args}                                    \
		${files}

# To use a # in a subshell you have to esacpe it twice (for make & for shell)
# to do this we put it in a varible
hash  := \#
#files := `find src -name '*.hs' | egrep -v 'exec|test/' | grep -v \${hash}`
files :=

ifdef $$files
files := $$files
endif
