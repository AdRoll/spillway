REBAR = rebar3

all: compile

deps:
	@$(REBAR) unlock
	@$(REBAR) upgrade

compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) doc

test: compile lint dialyzer xref ct cover

ct:
ifdef SUITE
	@$(REBAR) ct --suite=$(SUITE)
else
	@$(REBAR) ct
endif

clean:
	rm -rf _build

dialyzer:
	@$(REBAR) dialyzer

xref:
	@$(REBAR) xref

cover:
	@$(REBAR) cover

lint:
	@$(REBAR) lint
