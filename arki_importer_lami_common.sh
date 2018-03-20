log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

import_cosmo_fileinfo()
{
    sfile=${1##*/}
    sfile=${sfile%%.*}
    if [ "${sfile::3}" = "laf" ]; then # if laf use date given by model
	sdate=${sfile#laf}
	sdate=${sdate::10}
    else # use appended date
	sdate=${1%.grib}
	sdate=${sdate##*.}
    fi

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
    # interpolation on points | computation of derived variables
    # improve management of coordinates
    time vg6d_getpoint --output-format=native --network=temp \
	--lon=7.32000,7.76600,6.96600,7.85000,9.66667,10.33333,10.91667,10.60000,11.33333,12.05000,11.58333,12.20000,12.56667,13.00000,15.00000,9.00000,13.45250,7.61300,6.95000,8.80000,16.03333,13.18333,7.65000,9.28333,11.85000,8.85000,9.93333,10.70000,11.00000,11.61667,10.38333,12.43333,17.95000,12.50000,9.06667,7.66000,8.66000,8.60000,6.81000,7.06500,8.49000,8.30000,8.53900,9.32900,8.59600,9.11500,9.50100,8.53600,9.54200,12.56700,12.25000,11.78300,11.53300,11.00000,12.21700,11.88300,9.13038,9.21736,9.30434,9.12922,9.21631,9.30341,9.12805,9.21526,9.30247 \
	--lat=45.73700,45.60000,45.78300,45.47000,45.02775,44.80000,44.66667,44.71667,44.48333,44.21667,44.83333,44.41667,44.06667,45.00000,43.00000,44.00000,43.29920,44.53900,46.81667,41.91667,45.81667,46.03333,45.21667,45.44442,45.40000,44.41667,44.44442,44.21108,44.02775,44.65000,43.68333,41.65000,40.65000,37.91667,39.25000,45.00000,44.90000,45.90000,44.95000,45.14000,45.49000,46.12000,40.74300,40.32500,39.90100,39.22600,40.92400,39.31100,39.88000,45.58300,45.66700,45.08700,45.55000,45.43300,46.13300,45.41700,45.37065,45.37141,45.37209,45.43314,45.43390,45.43458,45.49563,45.49640,45.49708 \
	${1}_destag - | \
	v7d_transform --input-format=native --output-format=BUFR  \
	--output-variable-list=B10004,B11001,B11002,B11003,B11004,B11006,B12101,B13001 \
	- ${1}.bufr
    time eatmydata arki-scan --dispatch=$ARKI_CONF bufr:${1}.bufr > /dev/null
    rm -f ${1}_109 ${1}_110 ${1}_109_110 ${1}_destag ${1}.bufr
#	--output-variable-list=B10004,B11001,B11002,B11003,B11004,B11006,B12101,B12103,B13001,B13003 \
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

final_cleanup() {
    trap - EXIT
    exit
}

import_loop() {

    nonunique_exit

    # redirect all to logfile
    exec >>$LOGDIR/`basename $0`.log 2>&1
    set -x

    # security check
    [ -n "$ARKI_IMPROOT" ] || exit 1
    cd $ARKI_IMPROOT
    # set -e disabled because it fails when a file is arki-scanned
    # to error dataset and do not know yet in which occasions
    set +e

    mustexit=
    mustreload=
    trap '{ mustexit=Y; }' 15 20 2
    trap '{ mustreload=Y; }' 1
    trap '{ final_cleanup; }' EXIT

    while true; do
	donenothing=Y
	# tried with find -regex '.*/[^.][^/].*((?!tmp).)*$' or
	# '.*/[^.][^/].*\(?!tmp\).$' unsuccessfully
	for file in `find . -type f -name '[^.]*'|grep -v '\.tmp$'`; do
	    # do homework before classwork
	    [ -n "$mustexit" ] && exit 1 || true
	    [ -n "$mustreload" ] && exec "$0" "$@" || true
	    import_one $file && donenothing= || true
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

}
