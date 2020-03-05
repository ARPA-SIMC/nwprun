#!/bin/bash
# script for downloading radar volumes from DPC

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

dl_ftp() {
    # Create some variables for convenience
    YYYY=${DATE:0:4}
    DD=${DATE:6:2}
    MM=${DATE:4:2}
    hh=${TIME:0:2}
    mm=${TIME:2:2}
    NAME_FILE_FTP="${DD}-${MM}-${YYYY}-${hh}-${mm}.hdf"

    # Download radar volume for each radar in "RADAR_STATIONS" at $DATE$TIME
    for radstat in $RADAR_STATIONS; do
	filepath="${FTPDIR}/${radstat}/$NAME_FILE_FTP"
	ncftpget $ncftpauth . $filepath  || continue
	mv $NAME_FILE_FTP ${radstat}_${YYYY}${MM}${DD}${hh}${mm}.hdf
	import_signal_imported radar_vol $DATE$TIME $radstat
    done

    # Put on arkimet
#   waitfor="$waitfor `putarki_archive_and_wait odim $dlfilelist`"

## check whether anything has been downloaded
#    dlfilelist=""
#    shopt -s nullglob
#    for file in ????_$1.bz2; do
#	dlfilelist="$dlfilelist $file"
#    done
#    shopt -u nullglob
#
## something found, wait and retry (we don't know the file-creation policy)
## assuming we now the policy and do not retry
## please optimize
#    if [ -n "$dlfilelist" ]; then
#	sleep 180
#	ncftpget $ncftpauth . $filelist || true
#	dlfilelist=""
#	shopt -s nullglob
#	for file in ????_$1.bz2; do
#	    bunzip2 -c $file > ${file%.bz2}.bufr
#	    dlfilelist="$dlfilelist ${file%.bz2}.bufr"
#	done
#	shopt -u nullglob
## decommentare per archiviare
##	waitfor="$waitfor `putarki_archive_and_wait odim $dlfilelist`"
#	return 0
#    fi

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
. $NWPCONFBINDIR/nwpwait.sh
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

if [ -n "$1" ]; then
    dl_ftp $1 $2
else
    restore_state radar_vol_get.state	|| touch $NWPCONFDIR/$NWPCONF/radar_vol_get.state
    if [ -z "$DATE" -o -z "$TIME" ]; then 	# set minimum datetime
	mindate=`date -u --date '1 day ago' '+%Y%m%d 1200'`
	DATE=`date --date="$mindate" '+%Y%m%d'`
	TIME=`date --date="$mindate" '+%H%M'`
    else 	# increment datetime
	increment_datetime
    fi
    while true; do
#	# wait before querying the server
#	NWPWAITSOLAR_SAVE=$NWPWAITSOLAR
#	NWPWAITSOLAR=$NWPWAITSOLAR_RUN
	nwpwait_setup
	while nwpwait_wait; do
		:	
	done
#	# wait until data ready
#	NWPWAITSOLAR=$NWPWAITSOLAR_SAVE
#	nwpwait_setup
#	while nwpwait_wait; do
#	    if dl_ftp $DATE $TIME; then
#		import_signal_imported radar_vol $DATE$TIME
#		break
#	    fi
#	done

	dl_ftp $DATE $TIME  || true

	import_signal_imported radar_vol $DATE$TIME

	save_state radar_vol_get.state DATE TIME
	increment_datetime
	
#	exit 1

    done
fi
