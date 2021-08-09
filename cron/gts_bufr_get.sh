#!/bin/bash

# source common get_ procedures
. get_common.sh

# define custom functions
get_init() {
    export PROCNAME=gts_bufr_get
}

get_setup() {
    echo putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=bufr" "signal=gts_bufr"
}

get_cleanup() {
    echo putarki_configured_end $PROCNAME
}

get_one() {
    file="${FTPDIR}BUFR0001${DATE}${TIME:0:2}.DAT"
    rm -f $file
    ncftpget -V -f $WORKDIR_BASE/nwprun/.auth/bufr_ruc_get.cfg . $file || true
    if [ -f "$file" ]; then
	echo putarki_configured_archive $PROCNAME $file
	return 0
    fi
    return 1
}

# enter main loop
main_loop
