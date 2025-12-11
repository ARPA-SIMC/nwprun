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
# should we set ARKI_DLDIR in root conf.sh?
ARKI_DLDIR=$WORKDIR_BASE/download
# do not clean download directory on g100 until there is space (Iride project & c.)
if [ "$HPC_SYSTEM" = "g100" -o "$HPC_SYSTEM" = "leonardo" ]; then
    unset ARKI_DLDIR
fi
putarki_configured_dailycleanup 10 || true

log "cleaning arkimet datasets from expired data"
arki_dailycleanup $ARKI_CONF.main
# almost equivalent to
# arki-check --fix --repack --config=$ARKI_CONF.main

log "cleaning import signal files"
import_signal_dailycleanup 20 || true

log "rotating log files"
PATH=$PATH:/usr/sbin:/sbin
if which logrotate >/dev/null 2>&1; then
    if [ -f "$LOGDIR/logrotate.conf" ]; then
        log "really rotating log files"
	logrotate --state $LOGDIR/logrotate.status $LOGDIR/logrotate.conf
    fi
fi
