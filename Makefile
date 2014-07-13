APP = collective
REBAR = ./rebar
DIALYZER = dialyzer

DIALYZER_APPS = erts kernel stdlib crypto sasl 
DIALYZER_PLT = $(CURDIR)/.dialyzer_plt
DIALYZER_WARNINGS = -Wunmatched_returns -Werror_handling \
                    -Wrace_conditions #-Wunderspecs

.PHONY: all compile test clean get-deps build-plt dialyze generate

all: compile

compile:
	@$(REBAR) compile

test: compile
	@$(REBAR) ct skip_deps=true

clean:
	@$(REBAR) clean
	@rm -rf rel/$(APP)

get-deps:
	@$(REBAR) get-deps

build-plt:
	@$(DIALYZER) --build_plt --output_plt  $(DIALYZER_PLT) \
	    --apps $(DIALYZER_APPS) -r deps/*/ebin 

typer:
	typer -r ./src -I deps --plt $(DIALYZER_PLT)

dialyze: compile
	@$(DIALYZER) --src src -I deps/*/include -I deps --plt \
		$(DIALYZER_PLT) $(DIALYZER_WARNINGS)

generate: compile
	@rm -rf rel/$(APP)
	@(cd rel && ../$(REBAR) generate)
