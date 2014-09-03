#
 ERLANG_HOME ?= /opt/erlang/release/latest

 CC ?= /opt/gnu/gcc/4.7.3/bin/gcc

#
 REBAR_BIN  = ../bin/rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += CC="$(CC)"
 REBAR_ENV += ERL_LIBS=..

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

#
 ERL_ENV  =
 ERL_ENV += ERL_LIBS=sub_dirs:..

 ERL_OPT  =
 ERL_OPT += -sname $(1)@localhost
 ERL_OPT += -setcookie test
 ERL_OPT += -config priv/conf/$(1)

#PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts $(ERLANG_HOME)/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src sub_dirs/*/src
#DIALYZER_OPT += -I deps

#
default: all

#
delete-deps get-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

compile ct:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true


all: build

build: get-deps
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) -r deps

clean: delete-autosave $(foreach d,$(wildcard sub_dirs/*),$d.clean)
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

delete-autosave:
	@-find . -name "*~" | xargs rm -f

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

distclean: clean delete-deps
	@-rm -rf deps $(PLT)

sub_dirs/%:
	@(cd sub_dirs/$(basename $*); \
	  $(REBAR_ENV) $(realpath $(REBAR_BIN)) $(REBAR_OPT) $(subst .,,$(suffix $*)))

test: compile ct

#
n%: compile
	@$(ERL_ENV) $(ERLANG_HOME)/bin/erl $(call ERL_OPT,$@)

x%: compile
	@$(ERL_ENV) $(ERLANG_HOME)/bin/escript priv/escript/$@.escript


otp: otp_R15B03 otp_R16B otp_R16B01 otp_R16B02 otp_R16B03
otp_%:
	@echo "*** OTP: $* ***"
	@-ERLANG_HOME=/opt/erlang/release/$* $(MAKE) clean x1
