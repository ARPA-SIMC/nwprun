
create_static() {
    log "start create_static $1"
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
		log "create_static, data has changed"
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
    log "end create_static"
}

final_cleanup() {
    trap - EXIT
    exit
}

import_loop() {

    nonunique_exit

    # redirect all to logfile
    exec >>$LOGDIR/`basename $0`.log 2>&1
    # set -x

    # security check
    [ -n "$ARKI_IMPROOT" ] || exit 1
    cd $ARKI_IMPROOT
    # set -e disabled because it fails when a file is arki-scanned
    # to error dataset and do not know yet in which occasions
    # now enabled, check that all non serious errors are catched
    set -e

    mustexit=
    mustreload=
    trap '{ log "exit signal received"; mustexit=Y; }' 15 20 2
    trap '{ log "reload signal received"; mustreload=Y; }' 1
    trap '{ final_cleanup; }' EXIT

    while true; do
	donenothing=Y
	# tried with find -regex '.*/[^.][^/].*((?!tmp).)*$' or
	# '.*/[^.][^/].*\(?!tmp\).$' unsuccessfully
	# for sorting by creation time
	# find .  -printf "%T+\t%p\n" | sort | cut -f 2
	for file in `find . -type f -name '[^.]*'|grep -v '\.tmp$'`; do
	    # do homework before classwork
	    [ -n "$mustexit" ] && { log "exiting on signal"; exit 1; } || true
	    [ -n "$mustreload" ] && { log "reloading on signal"; exec "$0" "$@"; } || true
	    import_one $file && donenothing= || if [ "$?" != "1" ]; then exit 1; fi
	done
	# if something has been done do not cool down
	if [ -n "$donenothing" ]; then
	    # do homework before going to sleep
	    [ -n "$mustexit" ] && { log "exiting on signal"; exit 1; } || true
	    [ -n "$mustreload" ] && { log "reloading on signal"; exec "$0" "$@"; } || true
	    sleep $tmout
	    log "performing check"
	    periodic_check
	fi
    done

}

