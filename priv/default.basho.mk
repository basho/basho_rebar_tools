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
RM	?= /bin/rm -f

#
# The default edoc layout leaves the monospaced font tiny, so we simply
# append a tweak to the generated stylesheet.
#
cssfile := $(prj_dir)/doc/stylesheet.css
cssaddl := code,kbd,pre,tt { font-size: larger; }

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
	$(RM) -rf $(prj_dir)/doc/*

clean-dist ::

docs :: prereqs
	$(REBAR3) edoc
	@grep -q '$(cssaddl)' $(cssfile) || echo '$(cssaddl)' >> $(cssfile)

test :: prereqs
	$(REBAR3) as test do eunit

dialyzer :: prereqs
	$(REBAR3) as check do dialyzer

xref :: prereqs
	$(REBAR3) as check do xref

validate :: prereqs
	$(REBAR3) as validate do compile

veryclean :: clean clean-deps clean-dist clean-docs
	$(RM) -rf $(cache)

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
