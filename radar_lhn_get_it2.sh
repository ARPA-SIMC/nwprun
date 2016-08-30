#!/bin/bash

unset LANG
basedir=$HOME/ecflow
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/COSMO_IT2

set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/cosmo_model.sh
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/simc_site.sh
# end of setup
#set -x

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
    [ -n "$lastdate" ] || lastdate=`date -u --date '1 day ago' '+%Y%m%d12'`
    todate=`date -u '+%Y%m%d%H%M'`
    lastdate=`simc_create_radar_grib $lastdate $todate`
    [ -n "$lastdate" ] && save_state radar_lhn_get.state lastdate
fi

