#!/bin/sh

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
. $NWPCONFBINDIR/putarki.sh
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1
#set -x

# clean import
putarki_configured_dailycleanup 5

# clean import_sync and download
ARKI_IMPDIR=$WORKDIR_BASE/import_sync
ARKI_DLDIR=$WORKDIR_BASE/download
putarki_configured_dailycleanup 5
