#!/bin/sh

erl \
    -name ej2j@localhost \
    -config ej2j.config \
    -pa ebin \
    -boot start_sasl \
    -eval "[application:start(A) || A <- [crypto, public_key, ssl, exmpp, ej2j] ]"
