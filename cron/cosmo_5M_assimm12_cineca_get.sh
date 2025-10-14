#!/bin/bash

# source common get_ procedures
. `dirname $0`/cineca_get_common.sh

get_init() {
    export PROCNAME=assimm12_cineca_get
    export CONF_PREFIX=cosmo_5M/
    export ECF_MONITOR=Y
}

# enter main loop
main_loop "$@"
