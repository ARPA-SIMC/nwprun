#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=hres_am_foricon_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=hres_am_foricon"
    file_pattern="U3S????????????????1"
    file_processed=()
    file_list_file=${PROCNAME}_list.$$
    rm -f $file_list_file
    for hh in `seq $FIRST_BC_HH $FREQ_BC_HH $LAST_BC_HH`; do
	reftime=`datetime_cnmc $DATE$TIME`
	vertime=`datetime_add $DATE$TIME $hh`
	vertime=`datetime_cnmc $vertime`
	echo "U3S${reftime}${vertime}1"
    done > $file_list_file
    file_count=`cat $file_list_file|wc -l `

}

get_cleanup() {
    putarki_configured_end $PROCNAME
    rm -f $file_list_file
}

get_one() {
    while true; do
	donenothing=Y
	rclone --transfers 4 --include-from $file_list_file copy meteoam-cin:$FTPDIR/ .
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
	    return 0
	fi
	if [ -n "$donenothing" ]; then
	    return 1
	fi
    done
}

# enter main loop
main_loop "$@"
