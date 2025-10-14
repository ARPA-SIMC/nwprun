#!/bin/bash

# source common get_ procedures
. `dirname $0`/meteohub_get_common.sh

# define custom functions
get_init() {
    export PROCNAME=icon_2I
    export CONF_PREFIX=meteohub_get/
    export ECF_MONITOR=
}

# enter main loop
main_loop "$@"
