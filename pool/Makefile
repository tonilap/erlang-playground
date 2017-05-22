# Fallback to rebar on PATH
REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

co: $(REBAR3)
	@$(REBAR3) do clean, compile

test: co
	@$(REBAR3) eunit

all: test
	@$(REBAR3) ct, dialyzer

rel: co
	@$(REBAR3) release

dev: co
	@$(REBAR3) shell

prod: co
	@$(REBAR3) as prod tar
