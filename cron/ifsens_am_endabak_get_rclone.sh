#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=endabak_get
    export EXTRA_CONF=ifsens_am/
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=ifsens_am_endabak"
    file_pattern="U3X????????????????1"
    file_processed=()
    file_list_file=${PROCNAME}_list.$$
    rm -f $file_list_file
    reftime=`datetime_cnmc $DATE$TIME`
    for hh in 0 `seq 13 1 18`; do
	vertime=`datetime_add $DATE$TIME $hh`
	vertime=`datetime_cnmc $vertime`
	echo "U3X${reftime}${vertime}1"
    done > $file_list_file
    file_count=`cat $file_list_file|wc -l `

}

get_cleanup() {
    putarki_configured_end $PROCNAME
    rm -f $file_list_file
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    while true; do
	donenothing=Y
	rclone-am --transfers 4 --include-from $file_list_file copy meteoam-cin:$FTPDIR/ .
	for file in $file_pattern; do
	    alreadydone=
	    if [ -f "$file" ]; then
		for file_test in ${file_processed[@]}; do
		    if [ "$file" = "$file_test" ]; then
			alreadydone=Y
		    fi
		done
		if [ -z "$alreadydone" ]; then
		    # process $file
		    log "file $file successfully downloaded and unpacked"
		    putarki_configured_archive $PROCNAME ${file}
		    log "file ${file} successfully sent to archive"
		    file_processed[${#file_processed[@]}]=$file
		    donenothing=
		fi
	    fi
	done
	if [ "${#file_processed[@]}" -ge "$file_count" ]; then
	    return
	fi
	if [ -n "$donenothing" ]; then
	    false
	fi
    done
}

# enter main loop
main_loop "$@"
