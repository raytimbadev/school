MIN=$(shell find valid-test-sources -name '*.min' \! -name '*.pretty.min')

.PRECIOUS: %.c
.PHONY: all
all: $(patsubst %.min,%,$(MIN))

%.c: %.min
	cabal run $<

%: %.c
	gcc -o $@ $<

.PHONY: clean
clean:
	-rm -v $(patsubst %.min,%.c,$(MIN)) $(patsubst %.min,%.pretty.min,$(MIN)) $(patsubst %.min,%.symbol.txt,$(MIN)) $(patsubst %.min,%,$(MIN))
