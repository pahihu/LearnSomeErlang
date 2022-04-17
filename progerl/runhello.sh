#!/bin/sh
# erlc hello.erl
erl -noshell -pa $HOME/src/github/LearnSomeErlang/progerl \
    -s hello start -s init stop
