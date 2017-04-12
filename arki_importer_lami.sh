#!/bin/sh

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

make_itr()
{
    # area itr (~"lama")
    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coord \
	--ilon=6.1 --ilat=36. --flon=21. --flat=47.2 \
	$1 ${1}_itr
    time eatmydata arki-scan --dispatch=$ARKI_CONF grib:${1}_itr > /dev/null
#    rm -f ${1}_itr
# file is not removed in order to be used with the vertical profiles,
# find a better way
}

make_medl()
{

    time vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=index \
	--ix=1 --iy=4 --fx=1083 --fy=559 ${1} - | \
	vg6d_transform --trans-mode=s \
	--trans-type=boxregrid --sub-type=average --npx=4 --npy=4 \
	- ${1}_medl
    time eatmydata arki-scan --dispatch=$ARKI_CONF grib:${1}_medl > /dev/null
    rm -f ${1}_medl
}

make_prof()
{
    # equivalente (quasi, bisogna escludere qi) con grib_copy
#    grib_copy -w indicatorOfParameter=40,indicatorOfTypeOfLevel=109 $1 ${1}_109
#    grib_copy -w indicatorOfParameter=1/33/34/11/17/51,indicatorOfTypeOfLevel=110 $1 ${1}_110

    arki-query --data -o ${1}_109 \
	'level:GRIB1,109; product:GRIB1,,2,40;' \
	grib:$1
    arki-query --data -o ${1}_110 \
	'level:GRIB1,110; product:GRIB1,,2,33 or GRIB1,,2,34 or GRIB1,,2,11 or GRIB1,,2,17 or GRIB1,,2,51 or GRIB1,,2,1;' \
	grib:$1

    # interpolazione verticale
    vg6d_transform --component-flag=1 --trans-type=vertint --sub-type=linear \
	--trans-level-type=105,,105,105 \
	${1}_109 ${1}_109_110
    cat ${1}_109_110 >> ${1}_110
    cat $HOME/static/$2/last_110.grib >> ${1}_110 || true
    # destaggering u e v
    vg6d_transform --a-grid --anavariable-list=B10007 ${1}_110 ${1}_destag
    # interpolazione sui punti
    # ricalcolo delle variabili derivate e scrittura in BUFR
    time vg6d_getpoint --output-format=native --network=temp \
	${1}_destag - | \
	v7d_transform --input-format=native --output-format=BUFR  \
	--output-variable-list=B10004,B11001,B11002,B11003,B11004,B11006,B12101,B12103,B13001,B13003 \
	- ${1}.bufr
    time eatmydata arki-scan --dispatch=$ARKI_CONF bufr:${1}.bufr > /dev/null
    rm -f ${1}_109 ${1}_110 ${1}_109_110 ${1}_destag ${1}.bufr
}

import_one() {
#    trap '{ mustexit=Y; }' 15 20 2

    case $1 in
	*/PROD/*)
	    case $1 in
		*/lm5/*c) # dati constanti
		    log "start importing PROD/lm5/c $1"
# area itr (~"lama")
		    make_itr $1
		    rm -f ${1}_itr
# area medl
		    make_medl $1
		    log "done importing $1"
		    ;;
		*/lm5/*)
		    log "start importing PROD/lm5 $1"
# area itr (~"lama")
		    make_itr $1
# profili verticali
		    make_prof ${1}_itr cosmo_5M_itr
		    rm -f ${1}_itr
# area medl
		    make_medl $1
		    log "done importing $1"
		    ;;
	    esac
	    ;;
	./generic/*)
	    log "start importing generic $1"
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    log "done importing $1"
	    ;;
	./cosmo_2I_assim/*)
	    log "start importing cosmo_2I_assim $1"
#	    sdate=${1%.grib}
#	    sdate=${sdate##*.}
	    sfile=${1##*/}
	    sfile=${sfile%%.*}
	    sdate=${sfile#laf}
	    sdate=${sdate::10}
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    import_signal_imported cosmo_2I_assim $sdate $sfile
	    log "done importing $1"
	    ;;
	./cosmo_2I_fcast/*)
	    log "start importing cosmo_2I_fcast $1"
	    sdate=${1%.grib}
	    sdate=${sdate##*.}
	    sfile=${1##*/}
	    sfile=${sfn%%.*}
	    time eatmydata arki-scan --dispatch=$ARKI_CONF $1 > /dev/null
	    import_signal_imported cosmo_2I_fcast $sdate $sfile
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
	    sfn=${1##*/}
	    sfn=${sfn%%.*}
	    sdate=${sfn#*_}
	    sfile=${sfn%_*}
# go back 3 hours, check with Lucio why
	    import_signal_imported cnmc_cosmo_eps `datetime_sub $sdate 3` $sfile
	    log "done importing $1"
	    ;;
	./save/*)
	    cp -p $1 ~/save/
	    ;;
    esac
# after eatmydata make a sync on the filesystem to clean the soul
    sync # not all versions of sync support -f: -f $ARKI_CONF
    rm -f $1

#    trap 15 20 2
}

create_static() {
    ds=$1
    origin=
    if [ -n "$2" ]; then
	origin="origin:GRIB1,,,$2;"
    fi
    staticdir=$HOME/static/$ds
    mkdir -p $staticdir
    rm -f $staticdir/tmp.grib
    arki-query --data -o $staticdir/tmp.grib "Reftime:=yesterday 00:00; $origin product:GRIB1,80,2,8 or GRIB1,80,2,81; level:GRIB1,109 or GRIB1,1; timerange:GRIB1,0,0;" $ARKI_DIR/$ds
    if [ -s "$staticdir/tmp.grib" ]; then # got some data
	if [ -s "$staticdir/last.grib" ]; then # have already some data
	    if ! grib_compare -b yearOfCentury,month,day,hour,centuryOfReferenceTimeOfData $staticdir/tmp.grib $staticdir/last.grib; then # data have changed
		mv $staticdir/last.grib $staticdir/`date -u '+%Y%m%d'`.grib
		mv $staticdir/last_110.grib $staticdir/`date -u '+%Y%m%d'`_110.grib
		mv $staticdir/tmp.grib $staticdir/last.grib
		vg6d_transform --component-flag=1 \
		    --trans-type=vertint --sub-type=linear \
		    --trans-level-type=105,,105,105 \
		    $staticdir/last.grib $staticdir/last_110.grib
	    else
		rm -f $staticdir/tmp.grib
	    fi
	else # first time
	    mv -f $staticdir/tmp.grib $staticdir/last.grib
	    vg6d_transform --component-flag=1 \
		--trans-type=vertint --sub-type=linear \
		--trans-level-type=105,,105,105 \
		$staticdir/last.grib $staticdir/last_110.grib
	fi
    fi
}

periodic_check() {
# need a dataset cleanup?
    local now
    now=`date -u '+%Y%m%d'`
    if [ "$now" != "$lastcleanup" ]; then
#	trap '{ mustexit=Y; }' 15 20 2
	log "daily cleanup"
	arki_dailycleanup $ARKI_CONF
#	arki-check --fix --repack --config=$ARKI_CONF
	import_signal_dailycleanup 20 || true
	create_static cosmo_5M_itr 22
	lastcleanup=$now
#	trap 15 20 2
    fi
}

final_cleanup() {
    trap - EXIT
    exit
}

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

tmout=30
#lastcleanup=`date --date '1 day ago' -u '+%Y%m%d'`
lastcleanup=`date -u '+%Y%m%d'`
mustexit=
mustreload=

# security check
[ -n "$ARKI_IMPROOT" ] || exit 1
cd $ARKI_IMPROOT
# set -e disabled because it fails when a file is arki-scanned
# to error dataset and do not know yet in which occasions
set +e

trap '{ mustexit=Y; }' 15 20 2
trap '{ mustreload=Y; }' 1
trap '{ final_cleanup; }' EXIT

while true; do
    donenothing=Y
    for file in `find . -type f -name '[^.]*'`; do
# do homework before classwork
	[ -n "$mustexit" ] && exit 1 || true
	[ -n "$mustreload" ] && exec "$0" "$@" || true
	import_one $file
	donenothing=
    done
# if something has been done do not cool down
    if [ -n "$donenothing" ]; then
# do homework before going to sleep
	[ -n "$mustexit" ] && exit 1 || true
	[ -n "$mustreload" ] && exec "$0" "$@" || true
	sleep $tmout
	log "Performing check"
	periodic_check
    fi
done
