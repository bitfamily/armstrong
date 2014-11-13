.PHONY : compile test clean

compile: 
	./rebar get-deps
	./rebar compile

test: compile
	./rebar eu

clean:
	rm -f ./ebin/* .eunit/*
