#
 ERLANG_HOME ?= /opt/erlang/release/latest

#
 REBAR_BIN  = ../bin/rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += ERL_LIB=..

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

#
 ERL_OPT  =
#ERL_OPT += -sname $(1)
#ERL_OPT += -setcookie test
 ERL_OPT += -pa ebin deps/*/ebin
 ERL_OPT += -config files/$(1)
#ERL_OPT += -mnesia dir \"data/$(1)/mnesia\"

#PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts $(ERLANG_HOME)/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src
#DIALYZER_OPT += -I deps
 DIALYZER_OPT += -I ..

#
default: n1

#
delete-deps get-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

compile ct:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true


all: build

build: get-deps
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

clean: delete-autosave
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

delete-autosave:
	@-find . -name "*~" | xargs rm -f

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

distclean: clean delete-deps
	@-rm -rf deps $(PLT)

test: compile ct

#
n%: compile
#	@-mkdir -p data/$@/mnesia data/$@/logs
	@$(ERLANG_HOME)/bin/erl $(call ERL_OPT,$@)

x%: compile
	@ERL_FLAGS="" $(ERLANG_HOME)/bin/escript escript/$@.escript
