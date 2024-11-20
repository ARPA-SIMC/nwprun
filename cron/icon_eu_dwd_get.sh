#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

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
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    file="icon-eu_to_cleps_${DATE}${TIME:0:2}.grb2"
    rm -f $file

    wget -nv https://data.dwd.de/data/$file
    if [ -s "$file" ]; then
	putarki_configured_archive $PROCNAME $file
	return
    fi
    false
}

# enter main loop
main_loop "$@"
