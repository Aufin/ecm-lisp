#!/bin/sh
export GERBIL_PATH="/srv/ecm/app/.gerbil"

VARRUN=/srv/ecm/var/run/ecm-appd
VARLOG=/srv/ecm/var/log/ecm-appd
ETC=/srv/ecm/etc

mkdir -p $VARRUN
mkdir -p $VARLOG

CMD_HELP () {
	echo "Commands: status, help, shell, quit, exit"
}
	
# First, the gerbil httpd

HTTPD_PIDF="${VARRUN}/httpd.pid"
HTTPD_PROCESS_LOG="${VARLOG}/httpd-service.log"
run-httpd () {
    setsid sh -c "cd ${VARRUN}; umask 0; exec \"\$@\" </dev/null > $HTTPD_PROCESS_LOG 2>&1 & echo \$! > $HTTPD_PIDF" \
	   -- gerbil httpd -G /srv/ecm/app/.gerbil ensemble
}

httpd-pid () {
    [ -s "$HTTPD_PIDF" ] && cat "$HTTPD_PIDF"
}

check-httpd () {
	PID=`httpd-pid`
	[ -n $PID ] && kill -0 ${PID}
}

ensure-httpd () {
    check-httpd || run-httpd
}

# Now mostly a copperpasta for the old lisp app

LISP_PIDF="${VARRUN}/lisp/pid"
LISP_SOCK="${VARRUN}/lisp/sock"
# LISP_IN="${VARRUN}/lisp.in"
LISP_LOG="${VARLOG}/lisp/log"
LISP_DRIBBLE="${VARLOG}/lisp/dribble"



run-lisp () {
    mkdir -p `dirname $LISP_LOG`
    mkdir -p `dirname $LISP_PIDF`
    [ -s "$LISP_SOCK" ] && rm "$LISP_SOCK"
    [ -s "$LISP_PIDF" ] && rm "$LISP_PIDF"
    detachtty --dribble-file "$LISP_DRIBBLE" --log-file "$LISP_LOG" \
	       --pid-file "$LISP_PIDF" "$LISP_SOCK" /usr/local/bin/ecm-application
}

lisp-pid () {
    [ -s "$LISP_PIDF" ] && cat "$LISP_PIDF"
}

check-lisp () {
    PID=`lisp-pid`
    [ -n $PID ] && kill -0 ${PID} 
}

ensure-lisp () {
    check-lisp || run-lisp
}

# And finally a copasta with the caddy server

CADDY_CONF=/srv/ecm/app/etc/Caddyfile
CADDY_PIDF=/srv/ecm/var/run/caddy.pid
CADDY_SERVICE_LOG=/srv/ecm/var/log/caddy/service.log
CADDY_START_LOG=/srv/ecm/var/log/caddy/start.log

run-caddy () {
    # see https://caddyserver.com/docs/caddyfile
	mkdir -p `dirname $CADDY_START_LOG`
    caddy start --config $CADDY_CONF --watch --pidfile $CADDY_PIDF 2> $CADDY_START_LOG 
}

caddy-pid () {
    [ -s "$CADDY_PIDF" ] && cat "$CADDY_PIDF"
}

check-caddy () {
    PID=`caddy-pid`
    [ -n $PID ] && kill -0 ${PID} 
}

ensure-caddy () {
    check-caddy || run-caddy
}

caddy-show-log() {
  eval cat $CADDY_START_LOG $@
  eval cat $CADDY_SERVICE_LOG $@
}


clean () {
	false
}
show-status () {
    
    if check-httpd; then
	httpd_pid=`httpd-pid`
	echo "HTTPD is running! pid: $httpd_pid"
    else
	echo "HTTPD is not running."
    fi

    if check-lisp; then
	lisp_pid=`lisp-pid`
	echo "Lisp is running: $lisp_pid"
    else
	echo "Lisp is not running. Try the `$0 run-lisp` command"
    fi

	if check-caddy; then
	caddy_pid=`caddy-pid`
	echo "Caddy is running: $caddy_pid"
    else
	echo "Caddy is not running. Try the `$0 run-caddy` command"
    fi


}

dispatch-logs () {
	if [ "$#" -eq 0 ]; then
		caddy-show-log | jq
	else
		case "$1" in
		caddy)
			shift;
            caddy-show-log "$@";
			;;
		esac
	fi
}

dispatch () {
	echo DISPATCH $1 or $@;
	case "$1" in
		status)
			show-status;
			;;
		log|logs)
             shift; dispatch-logs $@
			;;
	
		shell)
			sh
			;;
		run-lisp)
			run-lisp
			;;
 		mux)
			mux $@
			;;
		quit)
			exec sh
			;;
		exit*|shutdown*)
			ret=`echo $inp| awk '{print $2}'`
			exit $ret;
			;;
		help|*)
			CMD_HELP;
			;;
	esac
	
}

mux () {
  while read -p "ecm-appd> " inp; do
	  dispatch $inp
  done
}

start () {
  mkdir -p "${VARRUN}"
  ensure-httpd > /dev/null 2>&1
  ensure-lisp > /dev/null 2>&1
  ensure-caddy > /dev/null 2>&1
  sleep 4.2
  show-status
  echo "Hit C-c for exit; $PS1 <--inter?"
  sleep infinity
  sh
}

graceful_exit () {
	echo "Cleaning up and exiting"
}
	
trap graceful_exit SIGTERM

if [ "$#" -eq 0 ]; then
	  start
else
	dispatch $@
fi
