#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=endabak_get
    export EXTRA_CONF=ifsens_am/
}

get_setup() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=ifsens_am_endabak"
#    file_pattern="U3S????????????????1"
# Create array of files to be downloaded
    reftime=`datetime_cnmc $DATE$TIME`
    for hh in 0 `seq 13 1 18`; do
        vertime=`datetime_add $DATE$TIME $hh`
        vertime=`datetime_cnmc $vertime`
	file_list[$hh]="U3X${reftime}${vertime}1"
    done
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    while true; do
	donenothing=Y

        for i in ${!file_list[@]}; do # ! extract keys from array
            file=${file_list[$i]}
	    filepath="${FTPDIR}/${file}"
            log "starting download of $filepath"
            ncftpget -V -f $WORKDIR_BASE/nwprun/.auth/meteoam_cineca.cfg . $filepath || continue
            if [ -s "$file" ]; then
                # process $file
                log "file $file successfully downloaded and unpacked"
                putarki_configured_archive $PROCNAME ${file}
                log "file ${file} successfully sent to archive"
                unset file_list[$i]
                donenothing=
            fi
        done

        if [ ${#file_list[@]} -eq 0 ]; then
            return
        fi
	if [ -n "$donenothing" ]; then
	    false
	fi
    done
}

# enter main loop
declare -A file_list # this must be here otherwise declare implies local
main_loop "$@"
