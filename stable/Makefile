#
# The easiest way to use this code is to copy it somewhere fresh and
# create a sibling directory ebin/

ERLC=erlc
ERLC_OPTS=-o ../ebin +nowarn_unused_vars
ERL_SRC = smart_exceptions.erl mapform0.erl smart_handler_b.erl smart_exc_rt.erl
OBJS := $(patsubst %.erl,../ebin/%.beam,$(ERL_SRC))

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

error:
	@echo "Make has been disabled for R9 to avoid clashes with regular code."
	@echo "Please look at Makefile for how to use this."

all:	$(OBJS)

clean:
	rm -f ../ebin/*

