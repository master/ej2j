#!/bin/sh

CONFIG=${1:-"ej2j.config"} 

erl \
    -name ej2j@localhost \
    -config $CONFIG \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -eval "[application:start(A) || A <- [alog, crypto, public_key, ssl, exmpp, ej2j] ]"
