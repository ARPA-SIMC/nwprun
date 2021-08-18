#!/bin/bash

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

unset LANG
basedir=$WORKDIR_BASE/nwprun
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod

#set -x
set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/arki_tools.sh
# end of setup

nonunique_exit
# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1

log "cleaning import and download directories"
ARKI_DLDIR=$WORKDIR_BASE/download
putarki_configured_dailycleanup 40
log "cleaning import_signal directories"
import_signal_dailycleanup 40
# arki_dailycleanup ???

log "rotating log files"
PATH=$PATH:/usr/sbin:/sbin
if which logrotate >/dev/null 2>&1; then
    if [ -f "$LOGDIR/logrotate.conf" ]; then
        log "really rotating log files"
	logrotate --state $LOGDIR/logrotate.status $LOGDIR/logrotate.conf
    fi
fi
