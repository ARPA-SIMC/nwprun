#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=radar_sri_get
}

get_setup() {
    TEMPLATE=`conf_getfile $TEMPLATE_NAME`
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=${TEMPLATE_NAME%.grib}"
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    unixdate=`date -u --date="$DATE $TIME" +%s000`
    rm -f sri.tif srill.grib srillmd.grib sriinter.grib
    log "starting download of SRI data for $DATE$TIME"
    curl -o sri.tif --silent --header 'Content-Type: application/json' \
	 --data '{"productType": "SRI", "productDate": '$unixdate'}' \
	 $SRI_URL

    if [ -f "sri.tif" ]; then
	log "SRI data for $DATE$TIME successfully downloaded"

	$SIMC_TOOLS vg6d_transform --output-format=grib_api \
		    --trans-type=metamorphosis --sub-type=settoinvalid \
		    --maskbounds=-10000.,-9998. \
		    gdal,,,,:sri.tif \
		    grib_api:$TEMPLATE:srill.grib

	# WMO standard: parameterCategory=1,parameterNumber=52 (mm/s), we must use DWD local table
	$SIMC_TOOLS grib_set -s dataDate=$DATE,dataTime=$TIME,discipline=0,parameterCategory=15,parameterNumber=195 \
		    srill.grib srillmd.grib

	$SIMC_TOOLS vg6d_transform --trans-type=boxinter --sub-type=average \
		    --output-format=grib_api:$TEMPLATE \
		    srillmd.grib sriinter.grib

	putarki_configured_archive $PROCNAME sriinter.grib
	log "SRI data for $DATE$TIME successfully processed and sent to archive"
	rm -f srill.grib srillmd.grib sriinter.grib
	return 0
    fi
}

# enter main loop
main_loop "$@"
