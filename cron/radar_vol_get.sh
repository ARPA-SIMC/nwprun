#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=radar_vol_get
    export ECF_MONITOR=Y
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=odim" "signal=radar_vol" "signalfile=Y"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
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
	return
    else
	false
    fi
}

# enter main loop
main_loop "$@"
