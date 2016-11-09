#!/bin/bash

unset LANG
basedir=$HOME/ope
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/RADAR_LHN_IT2

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

[ -n "$RADAR_LHN_WORKDIR" ] || exit 1
mkdir -p $RADAR_LHN_WORKDIR
rm -f $RADAR_LHN_WORKDIR/*
cd $RADAR_LHN_WORKDIR

if [ -n "$1" ]; then
    lastdate=$1
    if [ -n "$2" ]; then
	todate=$2
    else
	todate=`date -u '+%Y%m%d%H%M'`
    fi
    simc_create_radar_grib $lastdate $todate
else
    restore_state radar_lhn_get.state
    # initial date from end of previous run or yesterday at 12
    mindate=`date -u --date '1 day ago' '+%Y%m%d1200'`
    [ -n "$lastdate" -a "$lastdate" -gt "$mindate" ] || lastdate=$mindate
    todate=`date -u '+%Y%m%d%H%M'`
    lastdate=`simc_create_radar_grib $lastdate $todate`
    [ -n "$lastdate" ] && save_state radar_lhn_get.state lastdate
fi

