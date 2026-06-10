EMACS ?= emacs

compile:
	cask exec $(EMACS) -Q -batch			\
	-L .						\
	--eval '(setq byte-compile-error-on-warn t)'	\
	-f batch-byte-compile *.el

test:
	cask exec ert-runner

.PHONY: test
