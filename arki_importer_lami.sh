#!/bin/sh

unset LANG
basedir=$HOME/ope
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

nonunique_exit

# redirect all to logfile
exec >>$HOME/log/`basename $0`.log 2>&1
set -x
# set -e disabled because it fails when a file is arki-scanned
# to error dataset and do not know yet in which occasions
set +e

tmout=30
#lastcleanup=`date --date '1 day ago' -u '+%Y%m%d'`
lastcleanup=`date -u '+%Y%m%d'`
mustexit=

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

import_one() {
    trap '{ mustexit=Y; }' 15 20 2

    case $1 in
	*/PROD/*)
	    case $1 in
		*/lm5/*)
		    log "start importing PROD/lm5 $1"
		    noext=${1%.*}
		    ext=${1##*.}
# area itr (~"lama")
		    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
			--ilon=6.5 --ilat=36. --flon=21. --flat=47. \
			$1 ${noext}_itr.${ext}
		    time eatmydata arki-scan --dispatch=$ARKI_CONF ${noext}_itr.${ext} > /dev/null
		    rm -f ${noext}_itr.${ext}
# area medl
		    time vg6d_transform --trans-mode=s --trans-type=boxregrid --sub-type=average \
			--npx=4 --npy=4 $1 ${noext}_medl.${ext}
		    time eatmydata arki-scan --dispatch=$ARKI_CONF ${noext}_medl.${ext} > /dev/null
		    rm -f ${noext}_medl.${ext}
		    log "done importing $1"
		    ;;
	    esac
	    ;;
	./generic/*)
	    log "start importing generic $1"
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    log "done importing $1"
	    ;;
	./comet/*)
	    log "start importing comet $1"
	    tmpdir=`mktemp -d $ARKI_IMPROOT/tmptar.XXXXXXXXXX`
	    tar --transform='s?.*/??g' -C $tmpdir -xvf $1
	    for file in $tmpdir/*; do
		nmemb=${file##*lfff????0000_}
		nmemb=${nmemb%%_*.grb}
#		grib_set -s 'subCentre=98,setLocalDefinition=28,localDefinitionNumber=28,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=1,numberOfForecastsInEnsemble=20,baseDateEPS=20161130,baseTimeEPS=00,numberOfRepresentativeMember=0,numberOfMembersInCluster=20,totalInitialConditions=20' input.raw cleps.out
		grib_set -s "subCentre=98,setLocalDefinition=1,localDefinitionNumber=1,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=$nmemb,numberOfForecastsInEnsemble=20" $file $file.ls.grib

		eatmydata arki-scan --dispatch=$ARKI_CONF $file.ls.grib > /dev/null
		rm -f $file $file.ls.grib
	    done
	    safe_rm_rf $tmpdir
	    log "done importing $1"
	    ;;
    esac
    rm -f $1

    trap 15 20 2
}

periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
	trap '{ mustexit=Y; }' 15 20 2
	log "daily cleanup"
#	arki_dailycleanup $ARKI_CONF 3 12
	arki_dailycleanup $ARKI_CONF
	lastcleanup=$now
	trap 15 20 2
    fi
}

final_cleanup() {
    trap - EXIT
    exit
}

cd $ARKI_IMPROOT
# make a check before start
# periodic_check

trap '{ final_cleanup; }' EXIT

while true; do
    donenothing=Y
    for file in `find . -type f -name '[^.]*'`; do
	import_one $file
	donenothing=
    done
    [ -n "$mustexit" ] && exit 1 || true
# if something has been done do not cool down
    if [ -n "$donenothing" ]; then
	sleep $tmout
	log "Performing check"
	periodic_check
    fi
done
