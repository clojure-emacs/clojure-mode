.PHONY: clean compile lint test all
.DEFAULT_GOAL := all

clean:
	eldev clean

lint: clean
	eldev lint -c

# Checks for byte-compilation warnings.
compile: clean
	 eldev -dtT compile --warnings-as-errors

test: clean
	eldev -dtT -p test

all: clean compile lint test
