#!/bin/sh

### BEGIN INIT INFO
# Provides:          ej2j
# Required-Start:    $network $local_fs $named
# Required-Stop:
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
### END INIT INFO

PATH=/sbin:/usr/sbin:/bin:/usr/bin
NAME=`basename $0`
CONF_DIR=/etc/${NAME}
LOG_DIR=/var/log/${NAME}

test -f /etc/default/${NAME} && . /etc/default/${NAME}

case "$1" in
    start)
        hostname=`hostname -f`
        for t in $INSTANCES; do
            if [ -r "${CONF_DIR}/${t}.config" ]
            then
                echo "Starting ${LONGNAME} instance: ${t}"
                mkdir -p $LOG_DIR
                run_erl -daemon $LOG_DIR $LOG_DIR "erl \
-name ${t}@${hostname} \
-config ${CONF_DIR}/${t}.config \
-setcookie ${COOKIE} \
-boot start_sasl \
-eval '[application:start(A) || A <- [${APPS}] ]'"
            fi
        done
        ;;
    stop)
        for t in $INSTANCES; do
	    echo "Stopping ${LONGNAME}: ${t}"
            pkill -f "${CONF_DIR}/${t}.config"
        done    
        ;;
    restart)
        ;;
    *)
        echo "Usage: $0 {start|restart|stop}" >&2
        exit 1
    ;;
esac

exit 0
