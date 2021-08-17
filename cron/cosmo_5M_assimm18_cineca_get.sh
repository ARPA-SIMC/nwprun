#!/bin/bash

# source common get_ procedures
. `dirname $0`/cineca_get_common.sh

get_init() {
    export PROCNAME=assimm18_cineca_get
    export EXTRA_CONF=cosmo_5M/
}

# enter main loop
main_loop "$@"
