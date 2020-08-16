EMACS := emacs
ELFILES = $(shell find . -name '*.el' -type f)
ELCFILES = $(patsubst %.el, %.elc, $(ELFILES))
LOAD_PATH_OPT = -L ./init -L ./config -L ./util -L ./bootstrap
PACKAGE_DIR := ~/.emacs.d/elpa

.PHONY: build
build: $(ELCFILES)

.PHONY: rebuild-el
rebuild-el: clean build

.PHONY: rebuild-all
rebuild-all: clean clean-pkg build

.PHONY: clean
clean:
	-find . -name '*.elc' -type f | xargs -r rm

.PHONY: clean-pkg
clean-pkg:
	-rm -r $(PACKAGE_DIR)

%.elc: %.el
	$(EMACS) -batch $(LOAD_PATH_OPT) -f batch-byte-compile "$<"
