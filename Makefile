REBAR = rebar3

all: compile  xref

compile:
	@$(REBAR) compile

test: ct

ct:
	@rm -rf .ct
	@mkdir -p .ct
	@$(REBAR) compile
ifdef SUITE
	@$(REBAR) ct --suite=$(SUITE)
else
	@$(REBAR) ct
endif


clean:
	rm -rf _build*
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyze

xref:
	-@$(REBAR) xref skip_deps=true

