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
	mv $NAME_FILE_FTP ${radstat}.hdf #_${YYYY}${MM}${DD}${hh}${mm}.hdf
	putarki_configured_archive $1 ${radstat}.hdf
    done

    return 1
}

increment_datetime() {
    DATE=`date --date="$DATE $TIME $RADAR_VOL_STEP seconds" '+%Y%m%d'`
    TIME=`date --date="$DATE $TIME $RADAR_VOL_STEP seconds" '+%H%M'`
}

final_cleanup() {
    trap - EXIT
    exit
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

# setup kill traps (to be moved in a common code)
    mustexit=
    mustreload=
    trap '{ mustexit=Y; }' 15 20 2
    trap '{ mustreload=Y; }' 1
    trap '{ final_cleanup; }' EXIT


    while true; do
	nwpwait_setup
	while nwpwait_wait; do
# it's time for a break
	    [ -n "$mustexit" ] && exit 1 || true
            [ -n "$mustreload" ] && exec "$0" "$@" || true
	done

	dirname=radar_vol_$DATE$TIME.$$
	putarki_configured_setup $dirname "reftime=$DATE$TIME" "format=odim" "signal=radar_vol" "signalfile=Y"
	dl_ftp $dirname || true

	putarki_configured_end $dirname

	save_state radar_vol_get.state DATE TIME
	increment_datetime

    done
fi
