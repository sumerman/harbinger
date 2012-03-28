.PHONY: all compile clean eunit test eqc doc check dialyzer deps cleandeps

DIRS=src 

#all: compile eunit test doc
all: compile 

check: compile dialyzer

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	./rebar clean

cleandeps:
	./rebar delete-deps

eunit:
	./rebar eunit

test: eunit

doc:
	./rebar doc

dialyzer:
	./rebar skip_deps=true dialyze
