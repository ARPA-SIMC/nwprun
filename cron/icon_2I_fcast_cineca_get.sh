#!/bin/bash

# source common get_ procedures
. `dirname $0`/cineca_icon_get_common.sh

get_init() {
    export PROCNAME=fcast_cineca_get
    export EXTRA_CONF=icon_2I/
    export ECF_MONITOR=
}

icon_postproc() {
    IFS=_ read -ra keys <<< $1
    case ${keys[4]} in # verify [4]
    *Pv*)
        putarki_configured_archive $2 $1 grib
        tmpbase=$LAMI_CINECA_WORKDIR/${1}
        lami_make_crossg2 $1 ${tmpbase}_crossg2
        [ -s "${tmpbase}_crossg2" ] && putarki_configured_archive $2 ${tmpbase}_crossg2 $POSTPROC_FORMAT
        rm -f ${tmpbase}_crossg2
        lami_make_vprofg2 $1 ${tmpbase}_vprofg2
        [ -s "${tmpbase}_vprofg2" ] && putarki_configured_archive $2 ${tmpbase}_vprofg2 $POSTPROC_FORMAT
        rm -f ${tmpbase}_vprofg2
        ;;

     *A0* | *A1* | *A2* | *A3*)
        putarki_configured_archive $2 $1 grib
        ;;
    esac
}


set -x
export POSTPROC_FUNC=icon_postproc
# enter main loop
main_loop "$@"
