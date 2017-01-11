#!/bin/sh

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

import_one() {
    trap '' 15
    log "start importing $1"
    eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
    log "done importing $1"
    rm -f $1
    trap '{ exit 0; }' 15
}

periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
	log "daily cleanup"
	daily_cleanup
	lastcleanup=$now
    fi
}

daily_cleanup() {
    arki_dev=`stat -c %D $ARKI_DIR`
    for back in `seq 8 12`; do
	yy=`date -u --date "$back days ago" "+%Y"`
	mm=`date -u --date "$back days ago" "+%m"`
	dd=`date -u --date "$back days ago" "+%d"`
	log "cleaning $yy/$mm-$dd.grib1"
	for file in $ARKI_DIR/*/$yy/$mm-$dd.grib* $ARKI_DIR/*/$yy/$mm-$dd.bufr; do
	    if [ "`stat -c %D $file 2>/dev/null`" = "$arki_dev" ]; then
		rm -f $file
	    fi
	done
    done
    log "start arki-check"
    arki-check --fix --config=$ARKI_CONF # --repack
    log "done arki-check"
}

final_cleanup() {
    [ -n "$COPROC_PID" ] && kill $COPROC_PID
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
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$HOME/log/`basename $0`.log 2>&1
set -x

tmout=600
lastcleanup=`date -u '+%Y%m%d'`
[ -n "$ARKI_IMPDIR" ] || exit 1
cd $ARKI_IMPDIR
# set -e disabled because it fails when a file is arki-scanned
# to error dataset and do not know yet in which occasions
set +e
# make a check before start
periodic_check

trap '{ final_cleanup; }' EXIT

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
