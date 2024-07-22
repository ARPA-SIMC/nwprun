. `dirname $0`/get_common.sh


get_post() {
    . $NWPCONFBINDIR/lami_postproc.sh
    PROC_STEP=$(($MODEL_FREQINI*3600))
}

get_setup() {
    # $TIME is not known until now
    CINECA_SUITEDIR=`eval echo $CINECA_SUITEDIR_TMPL`
    mkdir -p $LAMI_CINECA_WORKDIR
    # g=global required to make statuslist visible to other functions
    declare -Ag statuslist
    statuslist=()
    foundrun=
    #putarki_configured_setup $MODEL_SIGNAL "reftime=$DATE$TIME" "signal=$MODEL_SIGNAL"
}

get_cleanup() {
    #putarki_configured_end $MODEL_SIGNAL
    safe_rm_rf $LAMI_CINECA_WORKDIR
}

get_one() {
    #    curdate=`grep 'ydate_ini *=' $CINECA_SUITEDIR/INPUT_ORG|sed -e "s/^.*'\([0-9]*\)'.*$/\1/g"`
    if [ -z "$foundrun" ]; then
        if [ -f "$CINECA_SUITEDIR/icon_${DATE}${TIME}_+00000000.rf" ]; then 
            foundrun=Y
            log "found run $PROCNAME $DATE$TIME"
        else
            return 1 # wait
        fi
    fi
    
# this seems to be the only place where it makes sense to do so ($DATE and $TIME change)
    . $NWPCONFBINDIR/nwptime.sh
    . $NWPCONFBINDIR/icon_model.sh
# this is done here in case the directory is removed and recreated
    cd $CINECA_SUITEDIR # /dataoutput
    putarki_configured_model_output $(($MODEL_STOP + 1))

}
