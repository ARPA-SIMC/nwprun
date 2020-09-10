#!/bin/bash

# special treatment, erase dir if exists
check_dir() {

    [ -n "$1" ] || return 1
    if [ -d "$ARKI_IMPDIR/configured/$1" ]; then
	safe_rm_rf $ARKI_IMPDIR/configured/$1
    fi
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
    # check MODEL_SIGNAL?
    dirname=${MODEL_SIGNAL}
    putarki_configured_setup $dirname "reftime=$DATE$TIME" "signal=$MODEL_SIGNAL"

    while true; do
# this is done here in case the directory is removed and recreated
        cd $CINECA_GRIBOUTDIR
        found=
# loop on ready-files
        shopt -s nullglob
        for rfile in $READYFILE_PATTERN; do
            if [ -z "${statuslist[$rfile]}" ]; then # it is a new file
                log "found ready-file $rfile"
# process all grib files related to $rfile
                for gfile in `model_readyfiletoname $rfile`; do
                    log "processing $gfile"
		    putarki_configured_archive $dirname $gfile grib
		    # create and archive postprocessed data if required
		    for ppc in ${POSTPROC_LIST[*]}; do
			ext=${ppc##*_}
			$ppc $gfile $LAMI_CINECA_WORKDIR/${gfile}_${ext}
			putarki_configured_archive $dirname $LAMI_CINECA_WORKDIR/${gfile}_${ext} $POSTPROC_FORMAT
			rm -f $LAMI_CINECA_WORKDIR/${gfile}_${ext}
		    done
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
                putarki_configured_end $dirname
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
    . $NWPCONFBINDIR/lami_postproc.sh
    # end of setup

    nonunique_exit # potrebbero esserci 5M e 2I

    # reset for testing
    set -x
    exec >>$LOGDIR/`basename $0`.log 2>&1

    # temporary to avoid double archiving, improve
    unset ARKI_IMPDIR
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
	# requested date is no more available, consider it done
	save_state lami_cineca_get.state DATETIME
    fi
    
    safe_rm_rf $LAMI_CINECA_WORKDIR
}

