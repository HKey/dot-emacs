EMACS := emacs
ELFILES = $(shell find . -name '*.el' -type f)
ELCFILES = $(patsubst %.el, %.elc, $(ELFILES))
LOAD_PATH_OPT = -L ./init -L ./config -L ./util -L ./bootstrap

.PHONY: build
build: $(ELCFILES)

.PHONY: rebuild
rebuild: clean build

.PHONY: clean
clean:
	-find . -name '*.elc' -type f | xargs -r rm

%.elc: %.el
	$(EMACS) -batch $(LOAD_PATH_OPT) -f batch-byte-compile "$<"
