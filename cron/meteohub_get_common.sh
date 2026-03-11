# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_setup() {
    if [ "$INTERACTIVE" = "N" ]; then
	local code
	code=0
	while [ "$code" = 0 ]; do # delete all old requests
	    log "trying to delete old data for request $MH_REQ"
	    meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg schedrm $MH_REQ: || code=$?
	done
    fi
    log "enabling request $MH_REQ"
    meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg schedenable $MH_REQ || true
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=$MODEL_SIGNAL"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
    # remove here rather than in get_one, safer if remove succeeds
    # server-side but returns an error for any reason
    meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg schedrm $MH_REQ:$DATE$TIME || true
    log "request $MH_REQ for $DATE$TIME removed"
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished

    # list available scheduled requests, if the query fails or the
    # desired request is not available (grep -q fails), the error trap
    # is called
    list=`meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg schedls`
    echo $list|grep -q "|$MH_REQ|.*|$DATE$TIME|"

    file=${MH_REQ}_${DATE}${TIME}.grib
    # download from meteohub
    mhlog=`meteo-hub-cli --verbose --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg \
        --output=$file scheddl $MH_REQ:$DATE$TIME`
    log "$mhlog"
    log "request $MH_REQ for $DATE$TIME downloaded successfully"

    if [ -s "$file" ]; then
	putarki_configured_archive $PROCNAME $file
    else
	false # if file is empty, retry
    fi

}

