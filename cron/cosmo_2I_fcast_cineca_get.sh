#!/bin/bash

# source common get_ procedures
. `dirname $0`/cineca_get_common.sh

get_init() {
    export PROCNAME=fcast_cineca_get
    export CONF_PREFIX=cosmo_2I/
    export ECF_MONITOR=Y
}

# enter main loop
main_loop "$@"
