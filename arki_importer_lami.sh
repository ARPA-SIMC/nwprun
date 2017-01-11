#!/bin/sh

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

import_one() {
    trap '{ mustexit=Y; }' 15 20 2

    case $1 in
	*/PROD/*)
	    case $1 in
		*/lm5/*)
		    log "start importing PROD/lm5 $1"
#		    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
		    noext=${1%.*}
		    ext=${1##*.}
		    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
			--ilon=6.5 --ilat=36. --flon=21. --flat=47. \
			$1 ${noext}_its.${ext}
		    time eatmydata arki-scan --dispatch=$ARKI_CONF ${noext}_its.${ext} > /dev/null
		    rm -f ${noext}_its.${ext}
		    log "done importing $1"
		    ;;
	    esac
	    ;;
	./generic/*)
	    log "start importing generic $1"
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    log "done importing $1"
	    ;;
    esac
    rm -f $1

    trap 15 20 2
}

periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
	trap '{ mustexit=Y; }' 15 20 2
	log "daily cleanup"
	arki_dailycleanup $ARKI_CONF 3 12
	lastcleanup=$now
	trap 15 20 2
    fi
}

final_cleanup() {
    trap - EXIT
    exit
}

unset LANG
basedir=$HOME/ope
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
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$HOME/log/`basename $0`.log 2>&1
set -x

tmout=30
#lastcleanup=`date --date '1 day ago' -u '+%Y%m%d'`
lastcleanup=`date -u '+%Y%m%d'`
mustexit=

# security check
[ -n "$ARKI_IMPROOT" ] || exit 1
cd $ARKI_IMPROOT
# set -e disabled because it fails when a file is arki-scanned
# to error dataset and do not know yet in which occasions
set +e
# make a check before start
# periodic_check

trap '{ final_cleanup; }' EXIT

while true; do
    donenothing=Y
    for file in `find . -type f -name '[^.]*'`; do
	import_one $file
	donenothing=
    done
    [ -n "$mustexit" ] && exit 1 || true
# if something has been done do not cool down
    if [ -n "$donenothing" ]; then
	sleep $tmout
	log "Performing check"
	periodic_check
    fi
done
