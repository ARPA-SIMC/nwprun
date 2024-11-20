#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=radar_dpc_lhn_get
}

get_setup() {
    for tmpl in ${TEMPLATE_LIST[*]}; do
	putarki_configured_setup ${tmpl%.grib} "reftime=$DATE$TIME" "format=grib" "signal=${tmpl%.grib}"
    done
}

get_cleanup() {
    for tmpl in ${TEMPLATE_LIST[*]}; do
        putarki_configured_end ${tmpl%.grib}
    done
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
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
    ncftpget -z -V -f $WORKDIR_BASE/nwprun/.auth/dpc.cfg . $filepath

    if [ -f "$fname" ]; then
	log "SRI data for $DATE$TIME successfully downloaded"

	for tmpl in ${TEMPLATE_LIST[*]}; do
	    tmplfile=`conf_getfile $tmpl`
	    modelname=${tmpl%%_*}
            rm -f $lfname
            $SIMC_TOOLS $NWPCONFBINDIR/sridpc_hdf52grib2.py --lhn_grid=$modelname \
			--grib_template=$tmplfile \
			--input_file=$fname \
			--output_file=$lfname >/dev/null


	    putarki_configured_archive ${tmpl%.grib} $lfname
	    rm -f $lfname
	done
	log "SRI data for $DATE$TIME successfully processed and sent to archive"
	return
    fi
    false
}

# enter main loop
main_loop "$@"
