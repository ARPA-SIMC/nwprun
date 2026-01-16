#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_init() {
    export PROCNAME=gts_bufrlc_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=bufr" "signal=gts_bufrlc"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    file="${FTPDIR}BUFR0001${DATE}${TIME:0:2}.DAT"
    rm -f $file
    ncftpget -V -f $WORKDIR_BASE/nwprun/.auth/bufr_ruc_get.cfg . $file
    if [ -f "$file" ]; then
	putarki_configured_archive $PROCNAME $file
	return
    fi
    false
}

# enter main loop
main_loop "$@"
