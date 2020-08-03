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

# clean import_sync
ARKI_IMPDIR=$WORKDIR_BASE/import_sync
unset ARKI_DLDIR
putarki_configured_dailycleanup 5

# clean import and download
ARKI_IMPDIR=$WORKDIR_BASE/import
ARKI_DLDIR=$WORKDIR_BASE/download
putarki_configured_dailycleanup 5
