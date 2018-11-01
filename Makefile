EMACS := emacs
ELFILES = $(shell find . -name '*.el' -type f)
ELCFILES = $(patsubst %.el, %.elc, $(ELFILES))
BYTE_COMPILE_INIT = ./script/byte-compile-init.el

.PHONY: build
build: $(ELCFILES)

.PHONY: rebuild
rebuild: clean build

.PHONY: clean
clean:
	-rm $(ELCFILES)

%.elc: %.el
	$(EMACS) -batch -l "$(BYTE_COMPILE_INIT)" -f batch-byte-compile "$<"
