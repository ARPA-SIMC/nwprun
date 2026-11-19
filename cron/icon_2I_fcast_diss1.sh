#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

get_init() {
    export PROCNAME=fcast_diss1
    export CONF_PREFIX=icon_2I/
    export ECF_MONITOR=
}

get_post() {
    PROC_STEP=$(($MODEL_FREQINI*3600))
}

get_setup() {
    :
}

get_cleanup() {
    :
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished

    TIME4=${TIME:0:2}00
    TIME2=${TIME:0:2}
    if [ "$TIME2" != "00" ]; then
	return
    fi
    L1="GRIB2S,150,0,47"
    L2="GRIB2D,150,0,47,150,0,48"
    LO="GRIB2S,1 or GRIB2S,103 or GRIB2D,100,,,,, or GRIB2D,106,,,,,"
    RAD="GRIB2,,0,4,198 or GRIB2,,0,4,199"

    # select levels
    for l in `seq 48 65`; do
	L1="$L1 or GRIB2S,150,0,$l"
	L2="$L2 or GRIB2D,150,0,$l,150,0,$((l+1))"
    done

    OUTFILE=icon_2I_${DATE}${TIME2}.grib
    OUTFILERAD=iconrad_2I_${DATE}${TIME2}.grib
    OUTDIR=out
    . $WORKDIR_BASE/nwprun/.auth/diss1_remote
    REMOTEPATH="$REMOTE/year=${DATE:0:4}/month=${DATE:4:2}/day=${DATE:6:2}/run=${TIME2}"
    DIR=$ARKI_DLDIR/configured/icon_2I_fcast:${DATE}${TIME4}::
    test -f "$DIR/end.sh"

    rm -f med.grib cum.grib tmp1.grib

    # retrieve averaged data and reaverage
    $SIMC_TOOLS arki-query --data 'timerange:Timedef,,0,' $DIR/icon_${DATE}${TIME4}_mlevSurf_ita2km_????_+????????.grb.grib > med.grib
    $SIMC_TOOLS vg6d_transform --comp-stat-proc=0:0 --comp-step='00 01' med.grib $OUTFILE

    $SIMC_TOOLS arki-query --data "timerange:Timedef,,0,;product:$RAD" $DIR/icon_${DATE}${TIME4}_surf_all2km_????_+????????.grb.grib > med.grib
    $SIMC_TOOLS vg6d_transform --comp-stat-proc=0:0 --comp-step='00 01' med.grib $OUTFILERAD

    # retrieve accumulated data and recumulate
    $SIMC_TOOLS arki-query --data 'timerange:Timedef,,1,' $DIR/icon_${DATE}${TIME4}_mlevSurf_ita2km_????_+????????.grb.grib > cum.grib
    $SIMC_TOOLS vg6d_transform --comp-stat-proc=1:1 --comp-step='00 01' cum.grib tmp1.grib
    cat tmp1.grib >> $OUTFILE

    # retrieve instantaneous data
    $SIMC_TOOLS arki-query --data "timerange:Timedef,,254,; level:$LO or $L1 or $L2" $DIR/icon_${DATE}${TIME4}_mlevSurf_ita2km_????_+????????.grb.grib >> $OUTFILE
    mkdir -p $OUTDIR
    $SIMC_TOOLS grib_copy $OUTFILE "$OUTDIR/${OUTFILE%.grib}_[endStep:l].grib"
    $SIMC_TOOLS grib_copy $OUTFILERAD "$OUTDIR/${OUTFILERAD%.grib}_[endStep:l].grib"

    rclone copy --include "${OUTFILE%.grib}_*.grib" $OUTDIR $REMOTEPATH
    safe_rm_rf $OUTDIR
    rm -f med.grib cum.grib tmp1.grib $OUTFILE $OUTFILERAD

}

set -x
# enter main loop
main_loop "$@"
