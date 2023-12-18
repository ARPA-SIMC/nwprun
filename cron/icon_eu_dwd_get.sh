#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=icon_eu_dwd_get
    export ECF_MONITOR=Y
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=icon_eu_dwd"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    file="icon-eu_to_cleps_${DATE}${TIME:0:2}.grb2"
    rm -f $file


    wget -nv https://data.dwd.de/data/$file || true
    if [ -f "$file" ]; then
	putarki_configured_archive $PROCNAME $file
	return 0
    fi
    return 1
}

# enter main loop
main_loop "$@"
