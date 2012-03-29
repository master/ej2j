#!/bin/sh

### BEGIN INIT INFO
# Provides:          ej2j
# Required-Start:    $network $local_fs $named
# Required-Stop:
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Starts EJ2J transport
# Description:       Starts MPP-to-XMPP Jabber Transport written in Erlang
### END INIT INFO

PATH=/sbin:/usr/sbin:/bin:/usr/bin

INSTANCES="j2j"
CONF_DIR=/etc/ej2j
LOG_DIR=/var/log/ej2j/
LONGNAME="EJ2J"
COOKIE=SALTIESTWATERS
APPLICAIONS="alog, exmpp, ej2j"

case "$1" in
    start)
        hostname=`hostname -f`
        for t in $INSTANCES; do
            if [ -r "${CONF_DIR}/${t}.config" ]
            then
                echo "Starting ${LONGNAME} instance: ${t}"
                mkdir -p $LOG_DIR/$NAME
                run_erl -daemon $LOG_DIR/$NAME $LOG_DIR/$NAME "erl \
-name ${t}@${hostname} \
-config ${CONF_DIR}/${t} \
-setcookie ${COOKIE} \
-boot start_sasl \
-eval '[application:start(A) || A <- [${APPLICAIONS}] ]'"
            fi
        done
        ;;
    stop)
        for t in $INSTANCES; do
	    echo "Stopping ${LONGNAME}: ${t}"
            pkill -f "${CONF_DIR}/${t}.config"
        done    
        ;;
    *)
        echo "Usage: $0 {start|stop}" >&2
        exit 1
    ;;
esac

exit 0