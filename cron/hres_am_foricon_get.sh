#!/bin/bash
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=hres_am_foricon_get
    export ECF_MONITOR=Y
}


get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=hres_am_foricon"
#    file_pattern="U3X????????????????1"
# Create array of files to be downloaded
    reftime=`datetime_cnmc $DATE$TIME`
    reftime_cineca=`datetime_add $DATE $TIME 6`
    UPLDIR_BAK=$UPLDIR/BCK/$reftime_cineca

    for hh in `seq $FIRST_BC_HH $FREQ_BC_HH $LAST_BC_HH`; do
        vertime=`datetime_add $DATE$TIME $hh`
        vertime=`datetime_cnmc $vertime`
        file_list[$hh]="U3S${reftime}${vertime}1"
    done
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    while true; do
        donenothing=Y

        for i in ${!file_list[@]}; do # ! extract keys from array
            file=${file_list[$i]}
            if [ -f "$UPLDIR/$file" ]; then
                # process $file
                log "file $file successfully downloaded and unpacked"
                putarki_configured_archive $PROCNAME $UPLDIR/${file}
                log "file ${file} successfully sent to archive"
                unset file_list[$i]
                donenothing=
            elif [ -f "$UPLDIR_BAK/$file" ]; then
                # process $file
                log "file $file successfully downloaded from backup and unpacked"
                putarki_configured_archive $PROCNAME $UPLDIR_BAK/${file}
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
