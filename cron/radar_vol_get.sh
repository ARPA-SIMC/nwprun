#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=radar_vol_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=odim" "signal=radar_vol" "signalfile=Y"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    # Create some variables for convenience
    YYYY=${DATE:0:4}
    DD=${DATE:6:2}
    MM=${DATE:4:2}
    hh=${TIME:0:2}
    mm=${TIME:2:2}
    name_file_ftp="${DD}-${MM}-${YYYY}-${hh}-${mm}.hdf"
    donesomething=

    # Download radar volume for each radar in "RADAR_STATIONS" at $DATE$TIME
    for radstat in $RADAR_STATIONS; do
	filepath="${FTPDIR}/${radstat}/$name_file_ftp"
	log "starting download of $filepath"
	ncftpget -V -f $WORKDIR_BASE/nwprun/.auth/dpc.cfg . $filepath || continue
	if [ -f "$name_file_ftp" ]; then
	    mv $name_file_ftp $radstat.hdf
	    putarki_configured_archive $PROCNAME $radstat.hdf
	    log "file $radstat.hdf successfully downloaded and sent to archive"
	    rm -f $radstat.hdf
	    donesomething=Y
	fi
    done
    if [ "$donesomething" = "Y" ]; then
	return 0
    else
	return 1
    fi
}

# enter main loop
main_loop "$@"
