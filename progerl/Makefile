.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = rhash

all: compile

compile: ${MODS:%=%.beam}

test: compile
	${ERL} -noshell -s rhash test -s init stop

clean:
	$(RM) *.beam erl_crash.dump

# vim:set noet:
