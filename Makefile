REBAR=./rebar3

all: compile

compile:
	@$(REBAR) compile

dialyzer:
	@$(REBAR) dialyzer

clean:
	@$(REBAR) clean -a

.PHONY: all deps compile clean
