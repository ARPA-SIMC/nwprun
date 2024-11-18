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
    
    # initialisations
    local rfile found
    while true; do
	# this is done here in case the directory is removed and recreated
	if [ ! -d "$CINECA_SUITEDIR/dataoutput" ]; then
	    false # the run has probably been interrupted, return and wait for a restart
	fi
        cd $CINECA_SUITEDIR/dataoutput
        found=
# loop on ready-files
        shopt -s nullglob
# this trick is required if pattern contains {*,?} because brace({,})
# expansion is done before variable expansion
        matchlist=`eval echo "$READYFILE_PATTERN"`
        for rfile in $matchlist; do
            if [ -z "${statuslist[$rfile]}" ]; then # it is a new file
                log "found new ready-file $rfile"
# process all grib files related to $rfile
                for gfile in `model_readyfiletoname $rfile`; do
                    log "processing $gfile"
		    putarki_configured_archive $MODEL_SIGNAL $gfile grib
		    # create and archive postprocessed data if required
		    for ppc in ${POSTPROC_LIST[*]}; do
			ext=${ppc##*_}
			$ppc $gfile $LAMI_CINECA_WORKDIR/${gfile}_${ext}
			[ "$retval" = "0" ] || false
			[ -s "$LAMI_CINECA_WORKDIR/${gfile}_${ext}" ] && putarki_configured_archive $MODEL_SIGNAL $LAMI_CINECA_WORKDIR/${gfile}_${ext} $POSTPROC_FORMAT
			rm -f $LAMI_CINECA_WORKDIR/${gfile}_${ext}
		    done
                done
# update status for $rfile
                statuslist[$rfile]="DONE"
                found=Y
            fi
        done
        shopt -u nullglob

        if [ -z "$found" ]; then # nothing new has been found
            if [ ${#statuslist[*]} -eq "$(($MODEL_STOP + 1))" ]; then
		retval=0 # end of run, consider not to set retval=0, in case some function has failed silently
		return
            else
		false
            fi
	fi
    done
}
