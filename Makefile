all: *.erl lib/*.erl
	make -C lib
	erl -compile *.erl lib/*.erl
	mv *.beam build

clean:
	rm -f build/*.beam build/*.dump

run_tests: all
	erl -noshell -eval "eunit:test(test_client), halt()"
