EMACS ?= emacs
CASK ?= cask

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ (format "%d.%d" emacs-major-version emacs-minor-version))')/elpa

.PHONY: travis-ci clean

test: 
	$(CASK) exec buttercup -L .

install:
	$(CASK) install

compile:
	$(CASK) exec $(EMACS) -batch -L -Q . -f batch-byte-compile *.el

travis-ci:
	$(CASK) exec $(EMACS) -batch -Q -l ci/leanote-mode-init.el

clean:
	rm -f *.elc

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
