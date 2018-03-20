#!/bin/sh

. `dirname $0`/arki_importer_lami_common.sh

import_one() {

    case $1 in
	*/PROD/marconi/*)
	    case $1 in
		*/lm5/*c) # dati constanti
		    log "start importing PROD/lm5/c $1"
# principale
		    time eatmydata arki-scan --dispatch=$ARKI_CONF grib:${1} > /dev/null
# area itr (~"lama")
		    make_itr $1
		    rm -f ${1}_itr
# area medl
		    make_medl $1
		    log "done importing $1"
		    ;;
		*/lm5/*)
		    log "start importing PROD/lm5 $1"
# principale
		    time eatmydata arki-scan --dispatch=$ARKI_CONF grib:${1} > /dev/null
# area itr (~"lama")
		    make_itr $1
# profili verticali
		    make_prof ${1}_itr cosmo_5M_itr
		    rm -f ${1}_itr
# area medl
		    make_medl $1
		    log "done importing $1"
		    ;;
		*/lm2.2/*)
		    log "start importing PROD/lm2.2 $1"
		    time eatmydata arki-scan --dispatch=$ARKI_CONF grib:${1} > /dev/null

		    log "done importing $1"
		    ;;
		*/swan/*)
		    log "start importing PROD/swan $1"
		    time eatmydata arki-scan --dispatch=$ARKI_CONF grib:${1} > /dev/null
		    log "done importing $1"
		    ;;
	    esac
	    ;;
	*)
	    return 1
	    ;;
    esac
    rm -f $1

}


periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
	log "daily cleanup"
	arki_dailycleanup $ARKI_CONF
#	arki-check --fix --repack --config=$ARKI_CONF
	import_signal_dailycleanup 20 || true
	create_static cosmo_5M_itr 22
	lastcleanup=$now
    fi
}

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod

set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/arki_tools.sh
# end of setup

ARKI_CONF=$ARKI_CONF.main
tmout=30
lastcleanup=`date -u '+%Y%m%d'`

import_loop
