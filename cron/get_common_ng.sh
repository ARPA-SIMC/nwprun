#!/bin/bash

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}


increment_datetime() {
    if [ -z "$DATE" -o -z "$TIME" ]; then # set minimum datetime
        DATE=`date -u --date '1 day ago' '+%Y%m%d'`
        TIME=1200
    else
        DATE=`date -u --date="$DATE $TIME $PROC_STEP seconds" '+%Y%m%d'`
        TIME=`date -u --date="$DATE $TIME $PROC_STEP seconds" '+%H%M'`
    fi
}


trap_setup() {
    # setup kill, exit and reload traps
    mustexit=
    mustreload=
    trap '{ mustexit=Y; log "exit requested, exiting as soon as possible"; }' 15 20 2
    trap '{ mustreload=Y; log "reload requested, reloading as soon as possible"; }' 1
    trap '{ final_cleanup; }' EXIT
}


trap_check() {
    # check if traps have to be called
    if [ -n "$mustexit" ]; then
       log "exiting on request"
       exit 1
    fi
    if [ -n "$mustreload" ]; then
       log "reloading on request"
       exec "$0" "$@"
    fi
}


final_cleanup() {
    trap - EXIT
    exit
}

# optional function, define a default one
get_post() {
    :
}


main_loop() {
    get_init

    unset LANG
    basedir=$WORKDIR_BASE/nwprun
    # setup common to user scripts
    # basic variables
    export NWPCONFDIR=$basedir/conf
    export NWPCONFBINDIR=$basedir/libexec/nwpconf
    export NWPCONF=prod/$EXTRA_CONF$PROCNAME

#    set -x
    set -e
    # source the main library module
    . $NWPCONFBINDIR/nwpconf.sh
    # source other optional modules
    . $NWPCONFBINDIR/putarki.sh
    . $NWPCONFBINDIR/arki_tools.sh
    . $NWPCONFBINDIR/nwpwait.sh
    if [ -n "$ECF_MONITOR" ]; then
	export ECF_NAME=/cron_get/${EXTRA_CONF////_}${PROCNAME%_get}
	export ECF_PASS=FREE
	export ECF_TIMEOUT=10 # probably not what i meant
	export ECF_TRYNO=0
	export ECF_JOBOUT=$LOGDIR/`basename $0`.log
    fi
    get_post
    # end of setup

    PROC_WORKDIR=$WORKDIR
    if [ -n "$1" ]; then # interactive run
	DATETIME=$1
	DATE=${DATETIME:0:8}
	TIME=${DATETIME:8:4}

	get_setup
	get_one && get_cleanup
    else # automatic run
	nonunique_exit
	# redirect all to logfile
	exec >>$LOGDIR/`basename $0`.log 2>&1
	# setup kill traps
	trap_setup
	safe_rm_rf $PROC_WORKDIR
	mkdir -p $PROC_WORKDIR
	cd $PROC_WORKDIR
	
	while true; do
	    restore_state $PROCNAME.state || touch $NWPCONFDIR/$NWPCONF/$PROCNAME.state
	    increment_datetime

	    # exit or wait if too early, depending on daemon mode
	    if [ "$DAEMON" = Y ]; then
		wait_run
	    else
		check_run || exit 0
	    fi
	    trap_check

	    [ -n "$ECF_MONITOR" ] && timeout_exec 5 $ecflow_client --init=$$ || true
	    [ -n "$ECF_MONITOR" ] && timeout_exec 5 $ecflow_client --label=currdate $DATE$TIME || true
	    log "starting download and archiving for $DATE$TIME"
	    nwpwait_setup
	    get_setup
	    while true; do
		get_one
		log "get_one returned: $retval"
		if [ "$retval" = 0 ]; then # improve $retval management
		    get_cleanup # correct here?
		    log "download and archiving for $DATE$TIME finished successfully"
         	    [ -n "$ECF_MONITOR" ] && timeout_exec 5 $ecflow_client --complete || true
		    break
		elif [ "$retval" = 2 ]; then
		    log "data for $DATE$TIME not available, but later data available, continuing"
         	    [ -n "$ECF_MONITOR" ] && timeout_exec 5 $ecflow_client --abort || true
		    break
		fi # else retval = 1 wait further
		if nwpwait_wait; then
		    trap_check
		else
		    log "download and archiving for $DATE$TIME did not finish successfully"
         	    [ -n "$ECF_MONITOR" ] && timeout_exec 5 $ecflow_client --abort || true
		    break
		fi
	    done
	    # get_cleanup
	    trap_check # do not save state if interrupted
	    save_state $PROCNAME.state DATE TIME
	    [ -n "$ECF_MONITOR" ] && timeout_exec 5 $ecflow_client --label=lastdate $DATE$TIME || true

	done
    fi
}
