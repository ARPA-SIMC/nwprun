#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=cosmo_am_enda_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=cosmo_am_enda"
    file_pattern="lfff000?0000_$DATE${TIME:0:2}.tar.bz2"
    file_processed=()
    file_count=3
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    while true; do
	donenothing=Y
	rclone copy meteoam-cin:$FTPDIR/ . --include "$file_pattern"
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
		    tmpdir=`mktemp -d $PWD/tmptar.XXXXXXXXXX`
		    tar --transform='s?.*/??g' -C $tmpdir -xvf $file
		    log "file $file successfully downloaded and unpacked"
		    for membfile in $tmpdir/*; do
			nmemb=${membfile##*l?ff????0000_}
			nmemb=${nmemb%%_*.grb}
			putarki_configured_archive $PROCNAME $membfile
			log "file $membfile successfully sent to archive"
			rm -f $membfile
		    done
		    safe_rm_rf $tmpdir
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
