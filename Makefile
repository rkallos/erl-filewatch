REBAR=./rebar3

all: compile

clean:
	@$(REBAR) clean -a

compile:
	@$(REBAR) compile

dialyzer:
	@$(REBAR) dialyzer

eunit:
	@$(REBAR) do eunit

test: eunit dialyzer

.PHONY: all deps compile clean
