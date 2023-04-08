#	Makefile for rebar3 lib 
all:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;
	rebar3 compile;
	cp src/*.api /home/joq62/erlang/infra/api_repo;
	rm -rf _build*;
	git add  *;
	git commit -m $(m);
	git push;
	echo Ok there you go!make
build:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;
	rm -rf  _build/test; # A bugfix in rebar3 or OTP
	rm -rf  _build;
	rebar3 compile;
	rm -rf _build

clean:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.* logs;
eunit:
	rm -rf  *~ */*~ src/*.beam test/*.beam erl_cra* config/*~;
	rm -rf _build ebin test_ebin;
	rm -rf common sd api;
	rm -rf Mnesia.*;
	rebar3 compile;
	mkdir test_ebin;
	mkdir api;
	cp src/*.api api;
	erlc -I api -I /home/joq62/erlang/infra/api_repo -o test_ebin test/*.erl;
	erl -pa _build/default/lib/*/* -pa test_ebin -sname do_test -run $(m) start $(a) $(b) -setcookie $(c)
