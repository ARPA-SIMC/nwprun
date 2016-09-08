#!/bin/bash
# script for downloading the new bufr provided by COMET (CNMCA)

dl_ftp() {

    filelist=""
    for obstype in $obstypes; do
	filelist="$filelist ${ftpdir}${obstype}_${1}.bz2"
    done

    ncftpget $ncftpauth . $filelist || true

# check whether anything has been downloaded
    dlfilelist=""
    shopt -s nullglob
    for file in ????_$1.bz2; do
	dlfilelist="$dlfilelist $file"
    done
    shopt -u nullglob

# something found, wait and retry (we don't know the file-creation policy)
    if [ -n "$dlfilelist" ]; then
	sleep 10
	ncftpget $ncftpauth . $filelist || true
	dlfilelist=""
	shopt -s nullglob
	for file in ????_$1.bz2; do
	    bunzip2 -c $file > ${file%.bz2}.bufr
	    dlfilelist="$dlfilelist ${file%.bz2}.bufr"
	done
	shopt -u nullglob
# decommentare per archiviare
	waitfor="$waitfor `putarki_archive_and_wait bufr $dlfilelist`"
	return 0
    fi
    return 1

}

unset LANG
basedir=$HOME/ope
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/BUFR_IT


set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
# end of setup

# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1

set -x

[ -n "$BUFR_WORKDIR" ] || exit 1
mkdir -p $BUFR_WORKDIR
rm -f $BUFR_WORKDIR/*
cd $BUFR_WORKDIR

# improve
ncftpauth="-f $basedir/.auth/meteoam.cfg"
ftpdir="BUFR/"
#obstypes="AIRC AMDA AMDN B002 B004 BUON OCEA PILN PILO RAOB SHIN SHIP SYNN SYNO TEMP"
obstypes="AIRC AMDA B002 B004 OCEA PILO SHIP SYNO TEMP"
waitfor=

if [ -n "$1" ]; then
    dl_ftp $1
else
    restore_state bufr_ruc_get.state
    # initial date from end of previous run or yesterday at 12
    mindate=`date -u --date '1 day ago' '+%Y%m%d1200'`
    [ -n "$lastdate" -a "$lastdate" -gt "$mindate" ] || lastdate=$mindate
    curdate=`datetime_now`
    lastdate=`datetime_add $lastdate 3`

    while [ "$lastdate" -lt "$curdate" ]; do
	dl_ftp $lastdate && save_state bufr_ruc_get.state lastdate
	lastdate=`datetime_add $lastdate 3`
    done
fi
# wait once for all to avoid useless pauses
[ -n "$waitfor" ] && putarki_wait_for_deletion $waitfor
