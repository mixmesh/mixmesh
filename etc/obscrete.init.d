#!/bin/sh
#

### BEGIN INIT INFO
# Provides:          obscrete
# Required-Start:    $all
# Required-Stop:     $all
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Starts obscrete
# Description:       Starts a
#                    longer description of obscrete
### END INIT INFO

set -e

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/etc/erlang/obscrete/obscrete.run
NAME="obscrete"
DESC="MixMesh"

test -x $DAEMON || exit 0

start()
{
    $DAEMON start
}

stop()
{
    $DAEMON stop
}

case "$1" in
    start)
	echo -n "Starting $DESC: "
	start
	echo "$NAME."
    ;;
    interactive)
	$DAEMON interactive
    ;;
    stop)
	echo -n "Stopping $DESC: "
	stop
	echo "$NAME."
    ;;
    status)
	$DAEMON status
    ;;
    reload|force-reload)
	echo "Reloading $DESC configuration files."
	$DAEMON hup
    ;;
    restart)
	echo -n "Restarting $DESC: "
	stop
	start
	echo "$NAME."
    ;;
    *)
	N=/etc/init.d/$NAME
	echo "Usage: $N {start|stop|status|interactive|restart|reload|force-reload}" >&2
	exit 1
    ;;
esac

exit 0
