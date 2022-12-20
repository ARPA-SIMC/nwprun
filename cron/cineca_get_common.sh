. `dirname $0`/get_common.sh


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
    #    curdate=`grep 'ydate_ini *=' $CINECA_SUITEDIR/INPUT_ORG|sed -e "s/^.*'\([0-9]*\)'.*$/\1/g"`
    if [ -z "$foundrun" ]; then
	curdate=`cut -d, -f1 $CINECA_SUITEDIR/*.description` || return 1
	if [ "$curdate" -gt "$DATE${TIME:0:2}" ]; then
	    return 2 # later date available
	elif [ "$curdate" -eq "$DATE${TIME:0:2}" ]; then
	    foundrun=Y
	else
	    return 1 # wait
	fi
    fi
    
    # initialisations
    local rfile found
    while true; do
# this is done here in case the directory is removed and recreated
        cd $CINECA_SUITEDIR/dataoutput
        found=
# loop on ready-files
        shopt -s nullglob
# this trick is required if pattern contains {*,?} because brace({,})
# expansion is done before variable expansion
        matchlist=`eval echo "$READYFILE_PATTERN"`
        for rfile in $matchlist; do
	    echo "READY:$rfile"
            if [ -z "${statuslist[$rfile]}" ]; then # it is a new file
                log "found ready-file $rfile"
# process all grib files related to $rfile
                for gfile in `model_readyfiletoname $rfile`; do
                    log "processing $gfile"
		    putarki_configured_archive $MODEL_SIGNAL $gfile grib
		    # create and archive postprocessed data if required
		    for ppc in ${POSTPROC_LIST[*]}; do
			ext=${ppc##*_}
			$ppc $gfile $LAMI_CINECA_WORKDIR/${gfile}_${ext}
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
                return 0 # end of run
            else
		return 1 # wait
            fi
	fi
    done
}
