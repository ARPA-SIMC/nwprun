#!/bin/bash
# script for downloading the new bufr provided by COMET (CNMCA)

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

dl_ftp() {

    filelist=""
    for obstype in $OBSTYPES; do
	filelist="$filelist ${FTPDIR}${obstype}_${1}.bz2"
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
# assuming we now the policy and do not retry
# please optimize
    if [ -n "$dlfilelist" ]; then
	sleep 180
	ncftpget $ncftpauth . $filelist || true
	dlfilelist=""
	shopt -s nullglob
	for file in ????_$1.bz2; do
	    bunzip2 -c $file > ${file%.bz2}.bufr
	    dlfilelist="$dlfilelist ${file%.bz2}.bufr"
	done
	shopt -u nullglob
# decommentare per archiviare
# this should probably be changed to (please change and test):
#	waitfor="$waitfor `putarki_archive bufr $dlfilelist`"
	waitfor="$waitfor `putarki_archive_and_wait bufr $dlfilelist`"
	return 0
    fi
    return 1

}

set -x
unset LANG
basedir=$OPE
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
. $NWPCONFBINDIR/arki_tools.sh
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1

set -x

safe_rm_rf $BUFR_WORKDIR
mkdir -p $BUFR_WORKDIR
cd $BUFR_WORKDIR

# improve
ncftpauth="-f $basedir/.auth/bufr_ruc_get.cfg"
waitfor=

if [ -n "$1" ]; then
    dl_ftp $1
else
    restore_state bufr_ruc_get.state
    # initial date from end of previous run or yesterday at 12
    mindate=`date -u --date '1 day ago' '+%Y%m%d12'`
    [ -n "$lastdate" -a "$lastdate" -gt "$mindate" ] || lastdate=$mindate
    curdate=`datetime_now`
    lastdate=`datetime_add $lastdate 3`

    while [ "$lastdate" -lt "$curdate" ]; do
	if dl_ftp $lastdate; then
	    save_state bufr_ruc_get.state lastdate
	    import_signal_imported cnmc_bufr $lastdate
	fi
	lastdate=`datetime_add $lastdate 3`
    done
fi
# wait once for all to avoid useless pauses
[ -n "$waitfor" ] && putarki_wait_for_deletion $waitfor
