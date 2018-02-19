#!/bin/bash

unset LANG
basedir=$HOME/nwprun
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=$1
shift

set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/simc_site.sh
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1

set -x

safe_rm_rf $WORKDIR
mkdir -p $WORKDIR
cd $WORKDIR

restore_state radar_lhn_get.state
# initial date from end of previous run or yesterday at 12
mindate=`date -u --date '1 day ago' '+%Y%m%d1200'`
[ -n "$lastdate" -a "$lastdate" -gt "$mindate" ] || lastdate=$mindate
todate=`date -u '+%Y%m%d%H%M'`
lastdate=`simc_create_radar_nc $lastdate $todate`
if [ -n "$lastdate" ]; then
    save_state radar_lhn_get.state lastdate
# loop on configurations
    for conf in "$@"; do
	# source the main library module
	NWPCONF=$conf
	. $NWPCONFBINDIR/nwpconf.sh
	# safe_rm_rf $WORKDIR
	mkdir -p $WORKDIR
	cd $WORKDIR
	simc_convert_radar_grib
    done
# remove remaining nc files
    simc_clean_radar_nc
fi

