# source common get_ procedures
. `dirname $0`/get_common_ng.sh

# define custom functions
get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=icon_eu_dwd"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
    # remove here rather than in get_one, safer if remove succeeds
    # server-side but returns an error for any reason
    meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg schedrm $MH_REQ:$DATE$TIME
    log "request $MH_REQ for $DATE$TIME removed"
}

get_one() {
    trap "retval=1; return 0" ERR
    # propagate the error trap to called functions
    set -o errtrace
    retval=0 # default return status: finished

    # list available scheduled requests, if the desired request is not
    # available grep -q fails and error trap is called
    list=`meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg schedls`
    echo $list|grep -q "|$MH_REQ|.*|$DATE$TIME|"

    file=${MH_REQ}_${DATE}${TIME}.grib
    # download from meteohub (consider --verbose?)
    meteo-hub-cli --configfile $WORKDIR_BASE/nwprun/.auth/mh.cfg \
		  --output=$file scheddl $MH_REQ:$DATE$TIME
    log "request $MH_REQ for $DATE$TIME downloaded successfully"

    if [ -s "$file" ]; then
	putarki_configured_archive $PROCNAME $file
    fi

}

