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
        (lami_make_crossg2 $1 ${tmpbase}_crossg2; [ -s "${tmpbase}_crossg2" ] && putarki_configured_archive $2 ${tmpbase}_crossg2 $POSTPROC_FORMAT; rm -f ${tmpbase}_crossg2) &
        (lami_make_vprofg2 $1 ${tmpbase}_vprofg2; [ -s "${tmpbase}_vprofg2" ] && putarki_configured_archive $2 ${tmpbase}_vprofg2 $POSTPROC_FORMAT; rm -f ${tmpbase}_vprofg2) &
        wait
        ;;

    *Pz*)
	if [ "${keys[3]}" = "unstr" ]; then # ignore all2km
# next part from postproc_icon.ecf with adaptations
	in_file=$PWD/${1}
	safe_rm_rf $LAMI_CINECA_WORKDIR/$1
	mkdir $LAMI_CINECA_WORKDIR/$1
	pushd $LAMI_CINECA_WORKDIR/$1
        rm -f hzero.grb topo.grb
        out_file=../${1/unstr/all2km}
        cp $(conf_getfile iconremap_hzero.nml) .
        cp $(conf_getfile template_all2km.grb) .
        $SIMC_TOOLS grib_copy -w typeOfFirstFixedSurface=4 $in_file hzero.grb
        $SIMC_TOOLS grib_copy -w typeOfFirstFixedSurface=1 $in_file topo.grb
        $SIMC_TOOLS /usr/libexec/ma_utils/math_grib.exe 1. hzero.grb 1. topo.grb hzero_msk.grb mskoutl -check=nil
        $MODEL_PRE_BINDIR/iconremap --remap_nml=iconremap_hzero.nml
        mv hzero_regular.grb $out_file
        putarki_configured_archive $2 $out_file grib
        rm -f *
	popd
	rmdir $LAMI_CINECA_WORKDIR/$1
	fi
        ;;

    *A0* | *A1* | *A2* | *A3*)
        putarki_configured_archive $2 $1 grib
        ;;
    esac
}


module load profile/archive
module load intel/oneapi-2021--binary
module load intelmpi/oneapi-2021--binary
module load netcdff/4.5.3--oneapi--2021.2.0-ifort
module load eccodes/2.21.0--intelmpi--oneapi-2021--binary

#set -x
export POSTPROC_FUNC=icon_postproc
# enter main loop
main_loop "$@"
