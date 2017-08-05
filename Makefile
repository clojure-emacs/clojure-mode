CASK = cask
export EMACS ?= emacs
EMACSFLAGS =
TESTFLAGS =

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean

elpa:
	$(CASK) install
	$(CASK) update
	touch $@

compile: $(OBJS)

clean:
	rm -f $(OBJS)

test: $(PKGDIR)
	$(CASK) exec ert-runner $(TESTFLAGS)

test-checks:
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/test-checks.el ./

test-bytecomp: $(SRCS:.el=.elc-test)

%.elc-test: %.el elpa
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		-l test/clojure-mode-bytecomp-warnings.el $
