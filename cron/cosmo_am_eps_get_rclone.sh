#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=cosmo_am_eps_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=cosmo_am_eps"
    file_pattern="lfff0???0000_??_$DATE${TIME:0:2}.grb.gz"
    file_processed=()
    file_count=360
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    while true; do
	donenothing=Y
	rclone --transfers 8 --include "$file_pattern" copy meteoam-cin:$FTPDIR/ .
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
		    gunzip -c $file > ${file%.gz} # keep original file
		    log "file $file successfully downloaded and unpacked"
		    putarki_configured_archive $PROCNAME ${file%.gz}
		    log "file ${file%.gz} successfully sent to archive"
		    rm -f ${file%.gz}
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
