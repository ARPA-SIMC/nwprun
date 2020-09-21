#!/bin/sh

. `dirname $0`/arki_importer_lami_common.sh

# format=grib|bufr|odim|
# reftime=reftime
# signal=signalname|
# signalfile=Y|

import_configured() {

    cd $1
    . ./start.sh
    if [ -n "$format" ]; then
	format="$format:"
    fi
    log "importing configured $format$2"
    time arki-scan --dispatch=$ARKI_CONF $format$2 > /dev/null
    if [ -n "$signalfile" -a -n "$signal" ]; then
	import_signal_imported "$signal" $reftime $2
    fi
    rm -f $2
}

import_configured_end() {

    cd $1
    . ./start.sh
    # if upload finished, check if the folder is empty and erase
    # at this stage i am authorised to remove rubbish
    rm -f .??*
    rm -f *.tmp
    if ! ls | grep -v '\.sh$'>/dev/null; then
	if [ -n "$signal" ]; then
	    import_signal_imported "$signal" $reftime
	fi
	rm -f *.sh
	cd -
	rmdir $1 || true # better leaving rubbish than failing
	log "completed importing configured folder $1"
    fi

}

import_one() {

    case $1 in
	./configured/*)
	    upfile=${1##*/}
	    updir=${1%/*}
	    case $upfile in
		start.sh)
		    return 1 # 1 = nothing done
		    ;;
		end.sh)
		    (import_configured_end $updir)
		    return 1 # 1 = nothing done
		    ;;
		*)
		    log "start importing configured $1"
# important! (..) is needed in order to use a subshell for not
# polluting the environment
		    (import_configured $updir $upfile)
		    log "done importing $1"
		    return
		    ;;
	    esac
	    ;;
	./generic/*)
	    log "start importing generic $1"
	    time arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    log "done importing $1"
	    ;;
	./comet/*)
# ./comet/lfff00060000_2017050506.tar.bz2
# ./comet/lbff00060000_2017050506.tar.bz2
# ./comet/fc_lfff02000000_2017050418.tar.bz2
	    log "skip importing comet $1"
	    return 1
	    tmpdir=`mktemp -d $ARKI_IMPROOT/tmptar.XXXXXXXXXX`
	    tar --transform='s?.*/??g' -C $tmpdir -xvf $1
	    for file in $tmpdir/*; do
		nmemb=${file##*l?ff????0000_}
		nmemb=${nmemb%%_*.grb}
#		grib_set -s 'subCentre=98,setLocalDefinition=28,localDefinitionNumber=28,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=1,numberOfForecastsInEnsemble=20,baseDateEPS=20161130,baseTimeEPS=00,numberOfRepresentativeMember=0,numberOfMembersInCluster=20,totalInitialConditions=20' input.raw cleps.out
		grib_set -s "subCentre=98,setLocalDefinition=1,localDefinitionNumber=1,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=$nmemb,numberOfForecastsInEnsemble=40" $file $file.ls.grib

		arki-scan --dispatch=$ARKI_CONF $file.ls.grib > /dev/null
		rm -f $file $file.ls.grib
	    done
	    safe_rm_rf $tmpdir
	    sfn=${1##*/}
	    sfn=${sfn%%.*}
# modificato per tenere conto delle segnalazioni dei forecast,
# per ora usiamo lo stesso dataset
	    sdate=${sfn#*l?ff????0000_}
	    sfile=${sfn%_*}
	    ds=${sfn%%_*}
	    if [ "$ds" = "fcmed" ]; then
		import_signal_imported cnmc_cosmo_eps_fcmed $sdate $sfile
		cp $1 /gpfs_arkimet/archive/cnmc_cosmo_eps_fcmed
	    else
		import_signal_imported cnmc_cosmo_eps $sdate $sfile
	    fi
	    log "done importing $1"
	    ;;
#	./save/*)
##	./*) # save-everything mode
#	    cp -p $1 ~/save/
#	    ;;
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
	log "performing daily cleanup"
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
# replace with nwpwait
tmout=30
lastcleanup=`date -u '+%Y%m%d'`

import_loop
