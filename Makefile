EMACS ?= emacs
EMACSLOADPATH := $(PWD):

.PHONY: test byte-compile ert-git ert-layout

test: byte-compile ert-git ert-layout

byte-compile:
	@EMACSLOADPATH=$(EMACSLOADPATH) $(EMACS) -Q --batch -l init.el -f batch-byte-compile etc/worktrees-*.el

ert-git:
	@EMACSLOADPATH=$(EMACSLOADPATH) $(EMACS) -Q --batch -L etc -l ert -l etc/worktrees-git-test.el -f ert-run-tests-batch-and-exit

ert-layout:
	@EMACSLOADPATH=$(EMACSLOADPATH) $(EMACS) -Q --batch -L etc -l ert -l etc/worktrees-layout-test.el -f ert-run-tests-batch-and-exit
