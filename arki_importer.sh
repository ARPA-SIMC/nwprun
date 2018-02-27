#!/bin/sh

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

import_one() {
    log "start importing $1"
    eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
    log "done importing $1"
    rm -f $1
}

periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
	log "daily cleanup"
	arki_dailycleanup $ARKI_CONF
#	arki-check --fix --repack --config=$ARKI_CONF
	import_signal_dailycleanup 20 || true
	create_static cosmo_5M_itr 22
	lastcleanup=$now
    fi
}

final_cleanup() {
#    [ -n "$COPROC_PID" ] && kill $COPROC_PID
    trap - EXIT
    exit
}

unset LANG
basedir=$OPE
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
exec >>$LOGDIR/`basename $0`.log 2>&1
set -x

tmout=30
#lastcleanup=`date --date '1 day ago' -u '+%Y%m%d'`
lastcleanup=`date -u '+%Y%m%d'`
mustexit=
mustreload=

[ -n "$ARKI_IMPROOT" ] || exit 1
cd $ARKI_IMPROOT
# set -e disabled because it fails when a file is arki-scanned
# to error dataset and do not know yet in which occasions
set +e
# make a check before start
periodic_check

trap '{ mustexit=Y; }' 15 20 2
trap '{ mustreload=Y; }' 1
trap '{ final_cleanup; }' EXIT

while true; do
    donenothing=Y
# tried with find -regex '.*/[^.][^/].*((?!tmp).)*$' or
# '.*/[^.][^/].*\(?!tmp\).$' unsuccessfully
    for file in `find . -type f -name '[^.]*'|grep -v '\.tmp$'`; do
# do homework before classwork
	[ -n "$mustexit" ] && exit 1 || true
	[ -n "$mustreload" ] && exec "$0" "$@" || true
	import_one $file
	donenothing=
    done
# if something has been done do not cool down
    if [ -n "$donenothing" ]; then
# do homework before going to sleep
	[ -n "$mustexit" ] && exit 1 || true
	[ -n "$mustreload" ] && exec "$0" "$@" || true
	sleep $tmout
	log "Performing check"
	periodic_check
    fi
done

# old code with inotify
__unused__() {
# remember files already queued for import, if very unlucky something
# could be missed here
#shopt -s nullglob
remain=`ls -tr1` # aggiungere un segnale USR1 per uscire in sicurezza
coproc inotifywait --monitor --exclude '^\./\..*' --format "%w%f" --event moved_to .

if [ -n "$remain" ]; then
    log "Importing remaining files"
    for file in $remain; do
	import_one $file
    done
    echo "Finished importing remaining files"
    remain=
fi

while true; do
    while read -u ${COPROC[0]} -t $tmout newfile; do
	import_one $newfile
    done
    log "Performing check"
    periodic_check
done

}
