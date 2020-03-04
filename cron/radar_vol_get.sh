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
#	waitfor="$waitfor `putarki_archive_and_wait odim $dlfilelist`"
	return 0
    fi
    return 1

}


increment_datetime() {
    DATE=`date --date="$DATE $TIME $RADAR_VOL_STEP seconds" '+%Y%m%d'`
    TIME=`date --date="$DATE $TIME $RADAR_VOL_STEP seconds" '+%H%M'`
}

set -x
unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/RADAR_VOL_IT


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

safe_rm_rf $RADAR_VOL_WORKDIR
mkdir -p $RADAR_VOL_WORKDIR
cd $RADAR_VOL_WORKDIR

# improve
ncftpauth="-f $basedir/.auth/radar_vol_get.cfg"
#waitfor=

if [ -n "$1" ]; then
    dl_ftp $1 $2
else
    restore_state radar_vol_get.state
    if [ -z "$DATE" -o -z "$TIME" ]; then # set minimum datetime
	mindate=`date -u --date '1 day ago' '+%Y%m%d 1200'`
	DATE=`date --date="$mindate" '+%Y%m%d'`
	TIME=`date --date="$mindate" '+%H%M'`
    else # increment datetime
	increment_datetime
    fi
    while true; do
	# wait before querying the server
	NWPWAITSOLAR_SAVE=$NWPWAITSOLAR
	NWPWAITSOLAR=NWPWAITSOLAR_RUN
	nwpwait_setup
	while nwpwait_wait; do
	done
	# wait until data ready
	NWPWAITSOLAR=NWPWAITSOLAR_SAVE
	nwpwait_setup
	while nwpwait_wait; do
	    if dl_ftp $DATE $TIME; then
		import_signal_imported radar_vol $DATE$TIME
		break
	    fi
	done
	save_state radar_vol_get.state DATE TIME
	increment_datetime
    done
fi
