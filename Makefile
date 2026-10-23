EMACS ?= emacs

.PHONY: test

test:
	$(EMACS) -Q --batch -L . -l test/goto-last-change-test.el -f ert-run-tests-batch-and-exit
