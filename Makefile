SOURCES = $(wildcard *.erl) $(wildcard lib/*.erl)

BEAMS = $(SOURCES:%.erl=%.beam)

all: $(BEAMS)

%.beam: %.erl
	erlc $<

.PHONY: clean

clean:
	-rm -f $(BEAMS)

run_tests: all
	erl -noshell -eval "eunit:test(test_client), halt()"
