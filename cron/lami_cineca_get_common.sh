#!/bin/bash

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}



make_itr()
{
    log "start make_itr $1"
    # --ilon=6.1 --ilat=36. --flon=21. --flat=47.2 \
    # --ilon=3.6 --ilat=33.8 --flon=23.5 --flat=49. \
    # area itr (~"lama")
    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
        --ilon=3.6 --ilat=33.8 --flon=23.5 --flat=49. \
        $1 $2
#    time arki-scan --dispatch=$ARKI_CONF grib:${1}_itr > /dev/null
#    rm -f ${1}_itr
# file is not removed in order to be used with the vertical profiles,
# find a better way
    log "end make_itr"
}

make_medl()
{
    log "start make_medl $1"
    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=index \
        --ix=1 --iy=4 --fx=1083 --fy=559 ${1} tmp.$$
    vg6d_transform --trans-mode=s \
        --trans-type=boxregrid --sub-type=average --npx=4 --npy=4 \
        tmp.$$ $2
    rm -f tmp.$$
    log "end make_medl"
}

make_prof()
{
     log "start make_prof $1"
# equivalente (quasi, bisogna escludere qi) con grib_copy
#    grib_copy -w indicatorOfParameter=40,indicatorOfTypeOfLevel=109 $1 ${1}_109
#    grib_copy -w indicatorOfParameter=1/33/34/11/17/51,indicatorOfTypeOfLevel=110 $1 ${1}_110

    arki-query --data -o ${1}_109 \
        'level:GRIB1,109; product:GRIB1,,2,40;' \
        grib:$1
    arki-query --data -o ${1}_110 \
        'level:GRIB1,110; product:GRIB1,,2,33 or GRIB1,,2,34 or GRIB1,,2,11 or GRIB1,,2,17 or GRIB1,,2,51 or GRIB1,,2,1;' \
        grib:$1

    # interpolazione verticale
    vg6d_transform --component-flag=1 --trans-type=vertint --sub-type=linear \
        --trans-level-type=105,,105,105 \
        ${1}_109 ${1}_109_110
    cat ${1}_109_110 >> ${1}_110
    cat $HOME/static/$2/last_110.grib >> ${1}_110 || true
    # destaggering u e v
    vg6d_transform --a-grid --anavariable-list=B10007 ${1}_110 ${1}_destag
    # interpolation on points | computation of derived variables
    # improve management of coordinates
    time vg6d_getpoint --output-format=native --network=temp \
        --lon=7.32000,7.76600,6.96600,7.85000,9.66667,10.33333,10.91667,10.60000,11.33333,12.05000,11.58333,12.20000,12.56667,13.00000,15.00000,9.00000,13.45250,7.61300,6.95000,8.80000,16.03333,13.18333,7.65000,9.28333,11.85000,8.85000,9.93333,10.70000,11.00000,11.61667,10.38333,12.43333,17.95000,12.50000,9.06667,7.66000,8.66000,8.60000,6.81000,7.06500,8.49000,8.30000,8.53900,9.32900,8.59600,9.11500,9.50100,8.53600,9.54200,12.56700,12.25000,11.78300,11.53300,11.00000,12.21700,11.88300,9.1881263,9.6687071,10.2229390,9.0854556,10.0261350,9.3900705,9.4978501,10.7976976,9.2730143,9.1566316,9.8693336,8.8263844,9.0097,9.2169,9.3717 \
        --lat=45.73700,45.60000,45.78300,45.47000,45.02775,44.80000,44.66667,44.71667,44.48333,44.21667,44.83333,44.41667,44.06667,45.00000,43.00000,44.00000,43.29920,44.53900,46.81667,41.91667,45.81667,46.03333,45.21667,45.44442,45.40000,44.41667,44.44442,44.21108,44.02775,44.65000,43.68333,41.65000,40.65000,37.91667,39.25000,45.00000,44.90000,45.90000,44.95000,45.14000,45.49000,46.12000,40.74300,40.32500,39.90100,39.22600,40.92400,39.31100,39.88000,45.58300,45.66700,45.08700,45.55000,45.43300,46.13300,45.41700,45.4636707,45.6947359,45.5397733,45.8119642,45.1334974,45.8529825,45.3128778,45.1603653,45.5840057,45.1858767,46.1712597,45.8176046,44.9936,45.8108,46.1372 \
        ${1}_destag - | \
        v7d_transform --input-format=native --output-format=BUFR  \
        --output-variable-list=B10007,B10004,B11001,B11002,B11003,B11004,B11006,B12101,B12103,B13001,B13003 \
        - ${1}.bufr
#    time arki-scan --dispatch=$ARKI_CONF bufr:${1}.bufr > /dev/null
    rm -f ${1}_109 ${1}_110 ${1}_109_110 ${1}_destag ${1}.bufr
    log "end make_prof"
}


# special treatment, erase dir if exists
check_dir() {

    [ -n "$1" ] || return 1
    if [ -d "$ARKI_IMPDIR/configured/$1" ]; then
	safe_rm_rf $ARKI_IMPDIR/configured/$1
    fi
}


output_setup() {

    # tolto .$$ da $dirname per semplificare il download
    dirname=${MODEL_SIGNAL}_$DATE$TIME
    check_dir $dirname
    putarki_configured_setup $dirname "reftime=$DATE$TIME" "format=grib" "signal=$MODEL_SIGNAL"

    case $MODEL_SIGNAL in
	cosmo_5M*)
#	dirname_itr=${MODEL_SIGNAL}_itr_$DATE$TIME.$$
#	putarki_configured_setup $dirname_itr "reftime=$DATE$TIME" "format=grib" "signal=${MODEL_SIGNAL}_itr"
	    dirname_vprof=${MODEL_SIGNAL}_vprof_$DATE$TIME
	    check_dir $dirname_vprof
	    putarki_configured_setup $dirname_vprof "reftime=$DATE$TIME" "format=bufr" "signal=${MODEL_SIGNAL}_vprof"
#	dirname_medl=${MODEL_SIGNAL}_medl_$DATE$TIME.$$
#	putarki_configured_setup $dirname_medl "reftime=$DATE$TIME" "format=grib" "signal=${MODEL_SIGNAL}_medl"
	    ;;
#	*)
#	    :
#	    ;;
    esac
}


output_process() {

    putarki_configured_archive $dirname $1
    case $MODEL_SIGNAL in
	cosmo_5M*)
	    cd $LAMI_CINECA_WORKDIR
	    make_itr $OLDPWD/$1 ${1}_itr
            putarki_configured_archive $dirname $LAMI_CINECA_WORKDIR/${1}_itr
#	make_prof ${1}_itr ${1}_vprof
#        putarki_configured_archive $dirname_vprof $LAMI_CINECA_WORKDIR/${1}_vprof
	    make_medl $OLDPWD/$1 $LAMI_CINECA_WORKDIR/${1}_medl
            putarki_configured_archive $dirname $LAMI_CINECA_WORKDIR/${1}_medl
# consider also e.g. ../dataoutput_med/$1
	    cd -
	    ;;
#	*)
#	    :
#	    ;;
    esac

}


output_end() {

    putarki_configured_end $dirname
    case $MODEL_SIGNAL in
	cosmo_5M*)
#        putarki_configured_end $dirname_itr
            putarki_configured_end $dirname_vprof
#        putarki_configured_end $dirname_medl
	    ;;
#	*)
#	    :
#	    ;;
    esac

}

putarki_configured_model_output_cineca() {

# initialisations
#    local workdir=$PWD
    local nrfiles=$1
    local rfile
    declare -A statuslist
    statuslist=()
    if [ "$ARKI_USE_INOTIFY" = Y ]; then
        NWPWAITWAIT=
    else
        NWPWAITWAIT=$PUTARKI_WAITSTART
    fi
    NWPWAITSOLAR=
    nwpwait_setup
    output_setup
#    dirname=${MODEL_SIGNAL}_$DATE$TIME.$$
#    putarki_configured_setup $dirname "reftime=$DATE$TIME" "format=grib" "signal=$MODEL_SIGNAL"

    while true; do
# this is done here in case the directory is removed and recreated
        cd $CINECA_GRIBOUTDIR
        found=
# loop on ready-files
        shopt -s nullglob
        for rfile in $READYFILE_PATTERN; do
            if [ -z "${statuslist[$rfile]}" ]; then # it is a new file
#                echo $rfile
# process all grib files related to $rfile
                for gfile in `model_readyfiletoname $rfile`; do
#                    echo $gfile
		    output_process $gfile
#                    putarki_configured_archive $dirname $gfile
                done
# update status for $rfile
                statuslist[$rfile]="DONE"
                found=Y
            fi
        done
        shopt -u nullglob

        if [ -n "$found" ]; then # something new has been found
            :
        else # nothing new has been found
# end of task condition
#           if [ "${statuslist[$1]}" = "DONE" ]; then
            if [ ${#statuslist[*]} -eq $nrfiles ]; then 
		output_end
#                putarki_configured_end $dirname
                return
            fi
# check end of time and wait if necessary (i.e. if not using inotify)
            nwpwait_wait
        fi
    done

}


lami_cineca_get() {
    set -e
    # temporary value
    DATE=20180101
    TIME=0000
    # source the main library module
    . $NWPCONFBINDIR/nwpconf.sh
    # source other optional modules
    . $NWPCONFBINDIR/nwptime.sh
    . $NWPCONFBINDIR/cosmo_model.sh
    . $NWPCONFBINDIR/arki_tools.sh
    . $NWPCONFBINDIR/nwpwait.sh
    . $NWPCONFBINDIR/putarki.sh
    # end of setup

    nonunique_exit # potrebbero esserci 5M e 2I

    # reset for testing
    set -x
    exec >>$LOGDIR/`basename $0`.log 2>&1

    restore_state lami_cineca_get.state || touch $NWPCONFDIR/$NWPCONF/lami_cineca_get.state

    if [ -z "$DATETIME" ]; then # set minimum datetime
	DATETIME=`date -u --date '1 day ago' '+%Y%m%d12'`
    else # increment datetime
	DATETIME=`datetime_add $DATETIME $MODEL_FREQINI`
    fi
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}
    # reinit the nwptime module in order to compute DATES and TIMES
    # based on new DATE and TIME
    nwptime_init
    # almost useless
    #    unset NWPWAITWAIT
    #    nwpwait_setup
    #    nwpwait_wait && exit 0 # too early, try next time

    # dirty trick since $TIME is not known until now
    CINECA_SUITEDIR=`eval echo $CINECA_SUITEDIR`
    CINECA_GRIBOUTDIR=`eval echo $CINECA_GRIBOUTDIR`
    # check datetime of available run    
    if [ ! -f "$CINECA_SUITEDIR/INPUT_ORG" ]; then # transition state
	exit 0
    fi
    curdate=`grep 'ydate_ini *=' $CINECA_SUITEDIR/INPUT_ORG|sed -e "s/^.*'\([0-9]*\)'.*$/\1/g"`

    mkdir -p $LAMI_CINECA_WORKDIR

    if [ "$curdate" -eq "$DATES$TIMES" ]; then
	putarki_configured_model_output_cineca $(($MODEL_STOP + 1))
	save_state lami_cineca_get.state DATETIME
    elif [ "$curdate" -gt "$DATES$TIMES" ]; then
	# requested date is no more available, switch to next run
	DATETIME=`datetime_add $DATETIME $MODEL_FREQINI`
	save_state lami_cineca_get.state DATETIME
    fi
    
    safe_rm_rf $LAMI_CINECA_WORKDIR
}

