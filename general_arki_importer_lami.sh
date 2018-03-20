#!/bin/sh

. `dirname $0`/arki_importer_lami_common.sh

import_one() {

    case $1 in
	./generic/*)
	    log "start importing generic $1"
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    log "done importing $1"
	    ;;
	./cosmo_2I_assim/*)
	    log "start importing cosmo_2I_assim $1"
# trust the filename for the reftime
	    sfile=${1##*/}
	    sfile=${sfile%%.*}
	    sdate=${sfile#laf}
	    sdate=${sdate::10}
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    import_signal_imported cosmo_2I_assim $sdate $sfile
	    log "done importing $1"
	    ;;
	./cosmo_2I_fcruc/*)
	    log "start importing cosmo_2I_fcruc $1"
# trust the additional date for the reftime
	    sdate=${1%.grib}
	    sdate=${sdate##*.}
	    sfile=${1##*/}
	    sfile=${sfile%%.*}
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    mkdir -p $dir_discarica/cosmo_2I_fcruc
	    arki-query --data "$query_discarica" $1 >> $dir_discarica/cosmo_2I_fcruc/verifica.grib
	    import_signal_imported cosmo_2I_fcruc $sdate $sfile
	    log "done importing $1"
	    ;;
	./cosmo_2I_fcens/*)
	    log "start importing cosmo_2I_fcens $1"
# trust the additional date for the reftime
	    sdate=${1%.grib}
	    sdate=${sdate##*.}
	    sfile=${1##*/}
	    sfile=${sfile%%.*}
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    import_signal_imported cosmo_2I_fcens $sdate $sfile
	    log "done importing $1"
	    ;;
	./comet/*)
# ./comet/lfff00060000_2017050506.tar.bz2
# ./comet/lbff00060000_2017050506.tar.bz2
# ./comet/fc_lfff02000000_2017050418.tar.bz2
	    log "start importing comet $1"
	    tmpdir=`mktemp -d $ARKI_IMPROOT/tmptar.XXXXXXXXXX`
	    tar --transform='s?.*/??g' -C $tmpdir -xvf $1
	    for file in $tmpdir/*; do
		nmemb=${file##*l?ff????0000_}
		nmemb=${nmemb%%_*.grb}
#		grib_set -s 'subCentre=98,setLocalDefinition=28,localDefinitionNumber=28,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=1,numberOfForecastsInEnsemble=20,baseDateEPS=20161130,baseTimeEPS=00,numberOfRepresentativeMember=0,numberOfMembersInCluster=20,totalInitialConditions=20' input.raw cleps.out
		grib_set -s "subCentre=98,setLocalDefinition=1,localDefinitionNumber=1,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=$nmemb,numberOfForecastsInEnsemble=20" $file $file.ls.grib

		eatmydata arki-scan --dispatch=$ARKI_CONF $file.ls.grib > /dev/null
		rm -f $file $file.ls.grib
	    done
	    safe_rm_rf $tmpdir
	    sfn=${1##*/}
	    sfn=${sfn%%.*}
# modificato per tenere conto delle segnalazioni dei forecast,
# per ora usiamo lo stesso dataset
	    sdate=${sfn#*l?ff????0000_}
	    sfile=${sfn%_*}
	    import_signal_imported cnmc_cosmo_eps $sdate $sfile
	    log "done importing $1"
	    ;;
	./save/*)
#	./*) # save-everything mode
	    cp -p $1 ~/save/
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
dir_discarica=/gpfs_arkimet/archive
query_discarica='level:GRIB1,105 or GRIB1,102 or GRIB1,1; product:GRIB1,,2,11 or GRIB1,,2,17 or GRIB1,,2,85 or GRIB1,,2,33 or GRIB1,,2,34 or GRIB1,,2,61 or GRIB1,,2,1 or GRIB1,,2,2'

import_loop
