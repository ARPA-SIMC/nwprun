#!/bin/bash
# script for downloading the new bufr provided by COMET (CNMCA)

dl_ftp() {

    reftime=`datetime_cnmc ${1}`
    filelist="${ftpdir}JGS${reftime}${reftime}1"
    localfilelist="JGS${reftime}${reftime}1"

    if ncftpget -V $ncftpauth . $filelist; then

# something downloaded, archive
	waitfor="$waitfor `putarki_archive_and_wait grib $localfilelist`"
	return 0
    fi
    return 1

}

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/GRIB_ME


set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1

set -x

[ -n "$GRIB_WORKDIR" ] || exit 1
mkdir -p $GRIB_WORKDIR
rm -f $GRIB_WORKDIR/*
cd $GRIB_WORKDIR

# improve
ncftpauth="-f $basedir/.auth/meteoam_cineca.cfg"
ftpdir="output/"
waitfor=

if [ -n "$1" ]; then
    dl_ftp $1
else
    restore_state grib_ecmwf_ana_get.state
    # initial date from end of previous run or yesterday at 12
    mindate=`date -u --date '1 day ago' '+%Y%m%d12'`
    [ -n "$lastdate" -a "$lastdate" -gt "$mindate" ] || lastdate=$mindate
    curdate=`datetime_now`
    lastdate=`datetime_add $lastdate 6`

    while [ "$lastdate" -lt "$curdate" ]; do
	dl_ftp $lastdate && save_state grib_ecmwf_ana_get.state lastdate || true
	lastdate=`datetime_add $lastdate 6`
    done
fi
# wait once for all to avoid useless pauses
[ -n "$waitfor" ] && putarki_wait_for_deletion $waitfor
