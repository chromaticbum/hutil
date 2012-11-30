all: deps compile

deps:
	./rebar get-deps

compile:
	./rebar compile

console: compile
	erl -pa ../hutil -pa ebin/ -pa deps/*/ebin -s hutil
