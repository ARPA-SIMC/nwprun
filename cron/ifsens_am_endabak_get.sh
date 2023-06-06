#!/bin/bash
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=endabak_get
    export EXTRA_CONF=ifsens_am/
    export ECF_MONITOR=Y
}


get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=ifsens_am_endabak"
#    file_pattern="U3X????????????????1"
# Create array of files to be downloaded
    reftime=`datetime_cnmc $DATE$TIME`
    for hh in `seq 13 1 18`; do
        vertime=`datetime_add $DATE$TIME $hh`
        vertime=`datetime_cnmc $vertime`
	file_list[$hh]="$FTPDIR/U3X${reftime}${vertime}1"
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
            if [ -f "$file" ]; then
                # process $file
                log "file $file successfully downloaded and unpacked"
                putarki_configured_archive $PROCNAME ${file}
                log "file ${file} successfully sent to archive"
		unset file_list[$i]
                donenothing=
            fi
	done

	if [ ${#file_list[@]} -eq 0 ]; then
	    return 0
	fi
        if [ -n "$donenothing" ]; then
            return 1
        fi
    done
}

# enter main loop
set -x
declare -A file_list # this must be here otherwise declare implies local
main_loop "$@"
