prj_dir	:= $(CURDIR)
cache	:= $(prj_dir)/.cache

dl_tgts	:=
#
# tools
#
ifeq ($(REBAR3),)
REBAR3	:= $(cache)/rebar3
dl_tgts	+= $(REBAR3)
endif
export	REBAR3
RM	?= /bin/rm

.PHONY	: check clean clean-deps clean-docs clean-dist compile dialyzer \
	  default docs prereqs test validate veryclean xref

default : compile

prereqs ::

compile :: prereqs
	$(REBAR3) as prod compile

check :: prereqs
	$(REBAR3) as check do brt-deps --check, dialyzer, xref

clean :: prereqs
	$(REBAR3) clean --all

clean-deps :: clean
	$(RM) -rf $(prj_dir)/_build

clean-docs ::
	$(REBAR3) as docs clean

clean-dist ::

docs :: prereqs
	$(REBAR3) edoc

dialyzer :: prereqs
	$(REBAR3) as check dialyzer

test :: prereqs
	$(REBAR3) eunit

validate :: prereqs
	$(REBAR3) as validate compile

veryclean :: clean clean-docs clean-deps clean-dist
	$(RM) -rf $(cache)

xref :: prereqs
	$(REBAR3) as check xref

#
# how to download files if we need to
#
ifneq ($(dl_tgts),)

dlcmd	:= $(shell which wget 2>/dev/null || true)
ifneq ($(wildcard $(dlcmd)),)
dlcmd	+= -O
else
dlcmd	:= $(shell which curl 2>/dev/null || true)
ifneq ($(wildcard $(dlcmd)),)
dlcmd	+= -o
else
$(error Need wget or curl to download files)
endif
endif

prereqs :: $(dl_tgts)

veryclean ::
	$(RM) -rf $(dl_tgts)

$(cache)/rebar3 :
	@test -d $(@D) || /bin/mkdir -p $(@D)
	@echo Downloading $@ ...
	@$(dlcmd) $@ https://s3.amazonaws.com/rebar3/rebar3
	@/bin/chmod +x $@

endif	# dl_tgts
