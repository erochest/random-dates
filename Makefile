
FLAGS=--enable-tests

all: init test docs package

init:
	cabal sandbox init
	make deps

test: build
	cabal test --test-option=--color

specs: build
	./dist/build/random-dates-specs/random-dates-specs

run:
	cabal run -- --n 25 --day 2014-10-23 --output dates.csv --text-file pg15.txt

help:
	cabal run -- --help

# docs:
# generate api documentation
#
# package:
# build a release tarball or executable
#
# dev:
# start dev server or process. `vagrant up`, `yesod devel`, etc.
#
# install:
# generate executable and put it into `/usr/local`
#
# deploy:
# prep and push

hlint:
	hlint *.hs src specs

clean:
	cabal clean

distclean: clean
	cabal sandbox delete

configure: clean
	cabal configure ${FLAGS}

deps: clean
	cabal install --only-dependencies --allow-newer ${FLAGS}
	make configure

build:
	cabal build

rebuild: clean configure build

.PHONY: all init test run clean distclean configure deps build rebuild hlint
