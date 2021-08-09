#!/bin/bash

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}


increment_datetime() {
    if [ -z "$DATE" -o -z "$TIME" ]; then # set minimum datetime
        DATE=`date -u --date '1 day ago' '+%Y%m%d'`
        TIME=1200
    else
        DATE=`date --date="$DATE $TIME $PROC_STEP seconds" '+%Y%m%d'`
        TIME=`date --date="$DATE $TIME $PROC_STEP seconds" '+%H%M'`
    fi
}


trap_setup() {
    # setup kill, exit and reload traps
    mustexit=
    mustreload=
    trap '{ mustexit=Y; }' 15 20 2
    trap '{ mustreload=Y; }' 1
    trap '{ final_cleanup; }' EXIT
}


trap_check() {
    # check if traps have to be called
    [ -n "$mustexit" ] && exit 1 || true
    [ -n "$mustreload" ] && exec "$0" "$@" || true
}


final_cleanup() {
    trap - EXIT
    exit
}


main_loop() {
    get_init

    unset LANG
    basedir=$WORKDIR_BASE/nwprun
    # setup common to user scripts
    # basic variables
    export NWPCONFDIR=$basedir/conf
    export NWPCONFBINDIR=$basedir/libexec/nwpconf
    export NWPCONF=prod/$PROCNAME

    set -x
    set -e
    # source the main library module
    . $NWPCONFBINDIR/nwpconf.sh
    # source other optional modules
    . $NWPCONFBINDIR/putarki.sh
    . $NWPCONFBINDIR/arki_tools.sh
    . $NWPCONFBINDIR/nwpwait.sh
    # end of setup

    PROC_WORKDIR=$WORKDIR
    # improve
    if [ -n "$1" ]; then # interactive run
#	PROC_WORKDIR=${PROC_WORKDIR}_interactive
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
	    
	    nwpwait_setup
	    get_setup
	    while true; do
		res=0
		get_one || res=$?
		if [ "$res" = 0 ]; then # improve $res management
		    get_cleanup # correct here?
		    break
		fi
		if nwpwait_wait; then
		    trap_check
		else
		    break
		fi
	    done
	    #    get_cleanup
	    save_state $PROCNAME.state DATE TIME
	done
    fi
}
