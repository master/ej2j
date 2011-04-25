#!/bin/sh

erl \
    -name ej2j@localhost \
    -config ej2j.config \
    -pa ebin \
    -boot start_sasl \
    -eval "application:start(exmpp), application:start(ej2j)"
