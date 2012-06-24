REBAR=`which rebar || ./rebar`

.PHONY: all test clean deps

all: deps compile

deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:	compile
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
distclean: clean
	@$(REBAR) delete-deps
dialyze:
	dialyzer --src -r src -I ebin \
	-pa deps/* \
	-Wunmatched_returns \
	-Werror_handling \
	-Wrace_conditions
