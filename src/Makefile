ERLC=erlc
ERLC_OPTS=-o ../ebin +nowarn_unused_vars
ERL_SRC = smart_exceptions.erl mapform0.erl
OBJS := $(patsubst %.erl,../ebin/%.beam,$(ERL_SRC))

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

# .app file?
all:	$(OBJS)

clean:
	rm -f ../ebin/*

