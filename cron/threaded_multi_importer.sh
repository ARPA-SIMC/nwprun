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
    log "importing $format$2"
    time $SIMC_TOOLS arki-scan --dispatch=$ARKI_CONF $format$2 > /dev/null || $SIMC_TOOLS arki-scan --dispatch=$ARKI_CONF $format$2 > /dev/null || true
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
	log "completed importing folder $1"
    fi

}

import_one() {

    case $1 in
	./configured.$IMPORT_THREAD/*/*)
	    [ -f "$1" ] || return 1 # was in _sync, needed?
	    upfile=${1##*/}
	    updir=${1%/*}
	    impdir=${updir%/*}
#	    impconf=$impdir/conf.sh
	    impsubdir=${updir##*/}
	    case $upfile in
		start.sh)
		    return 1 # 1 = nothing done
		    ;;
		end.sh)
		    (import_configured_end $updir)
		    return 1 # 1 = nothing done
		    ;;
		*)
		    log "start importing $1"
# important! (..) is needed in order to use a subshell for not
# polluting the environment
		    (
#			[ -f $impconf ] && safe_source $impconf
			cd $impdir
			excluded=
			included=
			for incl in ${INCLUDE[*]}; do
			    # next condition performs pattern matching
			    if [[ "$impsubdir/$upfile" == $incl ]]; then
				included=Y
			    fi
			done
			if [ -z "$included" ]; then
			    for excl in ${EXCLUDE[*]}; do
				# next condition performs pattern matching
				if [[ "$impsubdir/$upfile" == $excl ]]; then
				    excluded=Y
				fi
			    done
			fi
			if [ -z "$excluded" ]; then
			    import_configured $impsubdir $upfile
			else
			    log "$1 not requested, skipping"
			    rm -f $impsubdir/$upfile
			fi
		    )
		    log "completed importing $1"
		    return
		    ;;
	    esac
	    ;;
	./sync.$IMPORT_THREAD/*/*) # was sync_*
	    [ -f "$1" ] || return 1
	    upfile=${1##*/}
	    updir=${1%/*}
	    syncdir=${updir%/*}
#	    syncconf=$syncdir/conf.sh
	    syncsubdir=${updir##*/}
#	    [ -f "$syncconf" ] || return 1 # not a sync dir
	    case $upfile in
		start.sh)
		    if [ ! -f "$updir/started.sh" ]; then
			# -R to create remote dir
			(cd $syncdir; rsync -ptR --chmod=ug=rwX ./$syncsubdir/$upfile $SYNC_DEST)
#			(safe_source $syncconf; cd $syncdir; rsync -ptR --chmod=ug=rwX ./$syncsubdir/$upfile $sync_dest)
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
#				safe_source $syncconf
				cd $syncdir
				rsync -ptR --chmod=ug=rwX ./$syncsubdir/$upfile $SYNC_DEST
				if [ -n "$LOGSIM_SIGNAL_SYNC" -a -f "./$syncsubdir/start.sh" ]; then
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
			    log "completed syncing folder $updir"
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
			log "start syncing $1"
			(
#			    safe_source $syncconf
			    cd $syncdir
			    excluded=
			    included=
			    for incl in ${INCLUDE[*]}; do
				# next condition performs pattern matching
				if [[ "$syncsubdir/$upfile" == $incl ]]; then
				    included=Y
				fi
			    done
			    if [ -z "$included" ]; then
				for excl in ${EXCLUDE[*]}; do
				    # next condition performs pattern matching
				    if [[ "$syncsubdir/$upfile" == $excl ]]; then
					excluded=Y
				    fi
				done
			    fi
			    if [ -z "$excluded" ]; then
				    rsync -ptR --chmod=ug=rwX --remove-source-files ./$syncsubdir/$upfile $SYNC_DEST
			    else
			        log "$1 not requested, skipping"
				    rm -f $syncsubdir/$upfile
			    fi
			)
			log "completed syncing $1"
		    elif [ ! -f "$updir/start.sh" ]; then
			# erroneous situation, avoid repeating suddendly
			return 1 # should we remove $updir?
		    fi
		    return # 0 = something done or retry suddendly
		    ;;
	    esac
	    ;;
	*)
	    return 1
	    ;;
    esac
}


unset LANG
if [ -n "$1" ]; then
    thread=$1
else
    thread=$(basename $0)
    thread=${thread%%.*}
    if [ "$thread" = "threaded_multi_importer" ]; then
	echo "the script must be linked to <thread>.threaded_multi_importer.sh"
	echo "or receive the thread name as first command-line argument"
	exit 1
    fi
fi

basedir=$WORKDIR_BASE/nwprun
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/import/$thread

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

ARKI_CONF=$ARKI_CONF.$IMPORT_THREAD
# replace with nwpwait
tmout=30
lastcleanup=`date -u '+%Y%m%d'`

import_loop
