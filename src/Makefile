# ERLC_FLAGS += -pa ../ebin

# ok, a bit clumsy

all: ../ebin/smart_exceptions.beam ../ebin/mapform0.beam

../ebin/smart_exceptions.beam:
	erlc -o ../ebin smart_exceptions.erl

../ebin/mapform0.beam:
	erlc -o ../ebin mapform0.erl

clean:
	-rm -f ../ebin/*

