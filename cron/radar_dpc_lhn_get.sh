#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=radar_dpc_lhn_get
}

get_setup() {
    TEMPLATE=`conf_getfile $TEMPLATE_NAME`
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=${TEMPLATE_NAME%.grib}"
}

get_cleanup() {
        putarki_configured_end $PROCNAME
}

get_one() {
    # Create some variables for convenience
    Y=${DATE:0:4}
    m=${DATE:4:2}
    d=${DATE:6:2}
    H=${TIME:0:2}
    M=${TIME:2:2}
    fname="$d-$m-$Y-$H-$M.hdf"
    filepath="${FTPDIR}/${fname}"
    lfname=radar_$DATE$TIME.grib
    log "starting download of $filepath"
    ncftpget -z -V -f $WORKDIR_BASE/nwprun/.auth/dpc.cfg . $filepath || true

    if [ -f "$fname" ]; then
	log "SRI data for $DATE$TIME successfully downloaded"

        rm -f $lfname
        $SIMC_TOOLS $NWPCONFBINDIR/sridpc_hdf52grib2.py --lhn_grid=cosmo \
		    --grib_template=$TEMPLATE \
		    --input_file=$fname \
		    --output_file=$lfname >/dev/null


	putarki_configured_archive $PROCNAME $lfname
	log "SRI data for $DATE$TIME successfully processed and sent to archive"
	rm -f $lfname
	return 0
    fi
    return 1
}

# enter main loop
main_loop "$@"
