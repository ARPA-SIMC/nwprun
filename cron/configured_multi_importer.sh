#!/bin/sh

. `dirname $0`/multi_importer_common.sh

# format=grib|bufr|odim|
# reftime=reftime
# signal=signalname|
# signalfile=Y|


import_configured() {

    cd $1
    safe_source start.sh
    if [ -n "$format" ]; then
	format="$format:"
    fi
    if [ -n "$configext" ]; then
        ARKI_CONF=${ARKI_CONF%.*}.$configext
    fi
    log "importing configured $format$2"
    time $SIMC_TOOLS arki-scan --dispatch=$ARKI_CONF $format$2 > /dev/null || $SIMC_TOOLS arki-scan --dispatch=$ARKI_CONF $format$2 || true
    if [ -n "$signalfile" -a -n "$signal" ]; then
	if [ -n "$signal_method" ]; then
	    export IMPORT_SIGNAL_METHOD=$signal_method # override if requested
	fi
	import_signal_imported "$signal" $reftime $2
    fi
    rm -f $2
}

import_configured_end() {

    cd $1
    safe_source start.sh
    # if upload finished, check if the folder is empty and erase
    # at this stage i am authorised to remove rubbish
    rm -f .??*
    rm -f *.tmp
    if ! ls | grep -v '\.sh$'>/dev/null; then
	if [ -n "$signal" ]; then
	    if [ -n "$signal_method" ]; then
		export IMPORT_SIGNAL_METHOD=$signal_method # override if requested
	    fi
	    import_signal_imported "$signal" $reftime
	fi
	rm -f *.sh
	cd -
	rmdir $1 || true # better leaving rubbish than failing
	log "completed importing configured folder $1"
    fi

}

import_one() {

    case $1 in
	./configured/*/*)
	    [ -f "$1" ] || return 1 # was in _sync, needed?
	    upfile=${1##*/}
	    updir=${1%/*}
	    impdir=${updir%/*}
	    impconf=$impdir/conf.sh
	    impsubdir=${updir##*/}
	    case $upfile in
		start.sh)
#		    (import_configured_start $updir)
		    return 1 # 1 = nothing done
		    ;;
		end.sh)
		    (import_configured_end $updir)
		    return 1 # 1 = nothing done
		    ;;
		*)
		    log "start importing configured $1"
# important! (..) is needed in order to use a subshell for not
# polluting the environment
		    (
			[ -f $impconf ] && safe_source $impconf
			cd $impdir
			excluded=
			for excl in ${exclude[*]}; do
			    # next condition performs pattern matching
			    if [[ "$impsubdir/$upfile" == $excl ]]; then
				excluded=Y
			    fi
			done
			if [ -z "$excluded" ]; then
			    import_configured $impsubdir $upfile
			else
			    log "$1 not requested, skipping"
			    rm -f $impsubdir/$upfile
			fi
		    )
		    log "done importing $1"
		    return
		    ;;
	    esac
	    ;;
	./sync_*/*/*)
	    [ -f "$1" ] || return 1
	    upfile=${1##*/}
	    updir=${1%/*}
	    syncdir=${updir%/*}
	    syncconf=$syncdir/conf.sh
	    syncsubdir=${updir##*/}
	    [ -f "$syncconf" ] || return 1 # not a sync dir
	    case $upfile in
		start.sh)
		    if [ ! -f "$updir/started.sh" ]; then
			# -R to create remote dir
			(safe_source $syncconf; cd $syncdir; rsync -ptR --chmod=ug=rwX ./$syncsubdir/$upfile $sync_dest)
			touch $updir/started.sh
		    fi
		    return 1 # 1 = nothing done
		    ;;
		end.sh)
		    # if upload finished, check if the folder is empty and erase
		    # at this stage i am authorised to remove rubbish
		    if [ -f "$updir/started.sh" ]; then # is this necessary?
			rm -f $updir/.??* $updir/*.tmp
			if ! ls $updir | grep -v '\.sh$'>/dev/null; then
			    (
				safe_source $syncconf
				cd $syncdir
				rsync -ptR --chmod=ug=rwX ./$syncsubdir/$upfile $sync_dest
				if [ -n "$logsim_signal_sync" -a -f "./$syncsubdir/start.sh" ]; then
                                    safe_source ./$syncsubdir/start.sh
				    LOGSIM_PROCESS=$signal
                                    DATE=${reftime:0:8}
                                    TIME=${reftime:8:2}
				    simc_send_logevent ''
				    log "sending simc log event $LOGSIM_PROCESS"
				fi
			    )
			    rm -f $updir/*.sh
			    rmdir $updir || true # better leaving rubbish than failing
			    log "done syncing $updir"
			fi
		    fi
		    return 1 # 1 = nothing done
		    ;;
		*.sh)
		    # ignore other .sh
		    return 1 # 1 = nothing done
		    ;;
		*)
		    # sync only after start
		    if [ -f "$updir/started.sh" ]; then
			log "start syncing configured $1"
			(
			    safe_source $syncconf
			    cd $syncdir
			    excluded=
			    for excl in ${exclude[*]}; do
				# next condition performs pattern matching
				if [[ "$syncsubdir/$upfile" == $excl ]]; then
				    excluded=Y
				fi
			    done
			    if [ -z "$excluded" ]; then
				rsync -ptR --chmod=ug=rwX --remove-source-files ./$syncsubdir/$upfile $sync_dest
			    else
				rm -f $syncsubdir/$upfile
			    fi
			)
			log "done syncing $1"
		    elif [ ! -f "$updir/start.sh" ]; then
			# erroneous situation, avoid repeating suddendly
			return 1 # should we remove $updir?
		    fi
		    return # 0 = something done or retry suddendly
		    ;;
	    esac
	    ;;
	./generic/*)
	    log "start importing generic $1"
	    # improve error checking
	    time $SIMC_TOOLS arki-scan --dispatch=$ARKI_CONF $1 > /dev/null || true
	    rm -f $1
	    log "done importing $1"
	    ;;
	*)
	    return 1
	    ;;
    esac
#    rm -f $1
}


periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
	log "performing daily cleanup"
	arki_dailycleanup $ARKI_CONF
#	arki-check --fix --repack --config=$ARKI_CONF
	import_signal_dailycleanup 20 || true
	lastcleanup=$now
    fi
}

unset LANG
basedir=$WORKDIR_BASE/nwprun
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod

set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/arki_tools.sh
# better condition?
if [ -n "$SIMC_SITE" ]; then
    . $NWPCONFBINDIR/simc_site.sh
fi

# end of setup

ARKI_CONF=$ARKI_CONF.main
# replace with nwpwait
tmout=30
lastcleanup=`date -u '+%Y%m%d'`

import_loop
