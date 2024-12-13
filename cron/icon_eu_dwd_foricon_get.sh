#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=icon_eu_dwd_foricon_get
    export ECF_MONITOR=
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=icon_eu_dwd_foricon"
    # define file list in a naive way (without .bz2)
    i=0
    for dd in `seq 0 1`; do
	for hh in `seq 0 23`; do
            file_list[$i]=`printf "iefff%02d%02d0000" $dd $hh`
	    i=$(($i+1))
	done
    done
    dd=2
    for hh in `seq 0 3`; do
	file_list[$i]=`printf "iefff%02d%02d0000" $dd $hh`
	i=$(($i+1))
    done
    for hh in `seq 6 6 18`; do
	file_list[$i]=`printf "iefff%02d%02d0000" $dd $hh`
	i=$(($i+1))
    done
    file_list[$i]=iefff03000000

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

    if [ -z "$foundrun" ]; then
	icon_new=$(curl -s --netrc-file $WGETRC https://data.dwd.de/data/$DATE/out${TIME:0:2}/icon_new.bz2 | bunzip2 -c)
	# if not available the previous command calls error trap and returns
	# to be picky we should check here the contents of icon_new
	foundrun=Y
    fi

    while true; do
        donenothing=Y

        for i in ${!file_list[@]}; do # ! extract keys from array
            file=${file_list[$i]}
	    rm -f $file
	    log "starting download of $file"
	    curl -s --netrc-file $WORKDIR_BASE/nwprun/.auth/dwd.cfg https://data.dwd.de/data/$DATE/out${TIME:0:2}/$file.bz2 | bunzip2 -c > $file

	    if [ -s "$file" ]; then
                # process $file
                log "file $file successfully downloaded and unpacked"
                putarki_configured_archive $PROCNAME ${file}
                log "file $file successfully sent to archive"
                unset file_list[$i]
                donenothing=
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
main_loop "$@"
