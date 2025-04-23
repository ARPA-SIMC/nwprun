. `dirname $0`/get_common_ng.sh


get_post() {
# nwptime disabled, not necessary with description file
    # . $NWPCONFBINDIR/nwptime.sh
    . $NWPCONFBINDIR/cosmo_model.sh
    . $NWPCONFBINDIR/lami_postproc.sh
    # temporary to avoid double archiving, improve
    unset ARKI_IMPDIR
    PROC_STEP=$(($MODEL_FREQINI*3600))
}

get_setup() {
    # $TIME is not known until now
    CINECA_SUITEDIR=`eval echo $CINECA_SUITEDIR_TMPL`
    mkdir -p $LAMI_CINECA_WORKDIR
    # reinit the nwptime module in order to compute DATES and TIMES
    # based on new DATE and TIME
    # nwptime_init
    # g=global required to make statuslist visible to other functions
    declare -Ag statuslist
    statuslist=()
    foundrun=
    putarki_configured_setup $MODEL_SIGNAL "reftime=$DATE$TIME" "signal=$MODEL_SIGNAL"
}

get_cleanup() {
    putarki_configured_end $MODEL_SIGNAL
    safe_rm_rf $LAMI_CINECA_WORKDIR
}

get_one() {
    # this functions returns its state through the `retval` variable,
    # 3 values are possible at the moment:
    #  * retval=0 finished
    #  * retval=1 error or nothing new found, wait and retry until it
    #    is not too late
    #  * retval=2 nothing new found, but a successive run is available
    # the bash return value is always zero in order to avoid a forced
    # stop with set -e, this is achieved through the following error
    # trap whose default soft return value is 1, to force such a
    # return the intrinsic `false` command can be used; in order to
    # return a different state, set `retval` and use the `return`
    # command
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished
    #    curdate=`grep 'ydate_ini *=' $CINECA_SUITEDIR/INPUT_ORG|sed -e "s/^.*'\([0-9]*\)'.*$/\1/g"`
    if [ -z "$foundrun" ]; then
	curdate=`cut -d, -f1 $CINECA_SUITEDIR/*.description`
	if [ "$curdate" -gt "$DATE${TIME:0:2}" ]; then
	    retval=2 # later date available
	    return
	elif [ "$curdate" -eq "$DATE${TIME:0:2}" ]; then
	    foundrun=Y # proceed with function
	else
	    false # call err trap
	fi
    fi

    putarki_configured_model_output_get_one $(($MODEL_STOP + 1)) $CINECA_SUITEDIR/dataoutput $LAMI_CINECA_WORKDIR
}
