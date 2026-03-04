EMACS ?= emacs

DEPS = lsp-mode dash f s ht spinner markdown-mode lv
LOAD_PATH = $(foreach dep,$(DEPS),-L ~/.emacs.d/straight/build/$(dep))

test:
	$(EMACS) --batch $(LOAD_PATH) -L . -l alloy-lsp-test.el -f ert-run-tests-batch-and-exit

.PHONY: test
