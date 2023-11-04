
.PHONY: compile test clean elpa

clean:
	echo

# You can find a generic `eldev` installation script in https://github.com/emacs-eldev/eldev/blob/master/webinstall/eldev
# (Don't use the one defined for CircleCI in your local machine)

lint: clean
	eldev -c lint

# Checks for byte-compilation warnings.
compile: clean
	 eldev -dtT compile --warnings-as-errors

test: clean
	eldev -dtT -p test

all: compile test
