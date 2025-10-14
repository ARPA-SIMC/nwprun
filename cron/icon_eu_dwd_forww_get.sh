#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=icon_eu_dwd_forww_get
    export ECF_MONITOR=
}


get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=icon_eu_dwd_forww"
    # define file list in a naive way (without .bz2)
    if [ ${TIME:0:2} = "00" ]; then
	nh=72
    else
	nh=3
    fi
    #00/pmsl/icon-eu_europe_regular-lat-lon_single-level_2025100300_000_PMSL.grib2.bz2

    i=0
    for hh in `seq 0 $nh`; do
            file_list[$i]=`printf "${TIME:0:2}/pmsl/icon-eu_europe_regular-lat-lon_single-level_${DATE}${TIME:0:2}_%03d_PMSL" $hh`
	    i=$(($i+1))
            file_list[$i]=`printf "${TIME:0:2}/u_10m/icon-eu_europe_regular-lat-lon_single-level_${DATE}${TIME:0:2}_%03d_U_10M" $hh`
	    i=$(($i+1))
            file_list[$i]=`printf "${TIME:0:2}/v_10m/icon-eu_europe_regular-lat-lon_single-level_${DATE}${TIME:0:2}_%03d_V_10M" $hh`
	    i=$(($i+1))
    done
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    # it is already converted to _ng style with retval
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished

    while true; do
        donenothing=Y

        for i in ${!file_list[@]}; do # ! extract keys from array
            file=${file_list[$i]}.grib2
	    ofile=`basename $file`
	    rm -f $ofile
	    log "starting download of $file"
	    curl -s "$BASEURL/$file.bz2" | bunzip2 -c > $ofile

	    if [ -s "$ofile" ]; then
                # process $file
                log "file $ofile successfully downloaded and unpacked"
                putarki_configured_archive $PROCNAME ${ofile}
                log "file $ofile successfully sent to archive"
                unset file_list[$i]
                donenothing=
	    else
		break # if one file is missing the others are likely to be missing, avoid overloading the server
            fi
        done

        if [ ${#file_list[@]} -eq 0 ]; then
	    retval=0
            return
        fi
	if [ -n "$donenothing" ]; then
	    false # return and wait before retrying
	fi
    done
}

# enter main loop
declare -A file_list # this must be here otherwise declare implies local
set -x
main_loop "$@"
