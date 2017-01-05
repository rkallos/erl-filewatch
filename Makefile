REBAR=./rebar3

all: compile

compile:
	@$(REBAR) compile

dialyzer:
	@$(REBAR) dialyzer

clean:
	@$(REBAR) clean -a

check: compile dialyzer
	@bats t/stress.t

.PHONY: all deps compile clean check
