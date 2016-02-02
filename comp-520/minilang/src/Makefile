SRCDIR=valid-test-sources
BINDIR=bin-mini
MIN=$(shell find $(SRCDIR) -name '*.min' \! -name '*.pretty.min')
BIN=$(patsubst $(BINDIR)/%,$(SRCDIR)/%.min,$(MIN))

$(shell mkdir -p $(BINDIR))

.PRECIOUS: %.c
.PHONY: all
all: $(patsubst $(SRCDIR)/%.min,$(BINDIR)/%,$(MIN))

%.c: %.min
	cabal run $<

$(BINDIR)/%: $(SRCDIR)/%.c
	gcc -o $@ $<

.PHONY: clean
clean:
	-rm -v $(patsubst %.min,%.c,$(MIN)) $(patsubst %.min,%.pretty.min,$(MIN)) $(patsubst %.min,%.symbol.txt,$(MIN)) $(BIN)
