#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=cosmo_am_eps_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=cosmo_am_eps"
# Create array of files to be downloaded
    file_list=()
    for hh in `seq $FIRST_BC_HH $FREQ_BC_HH $LAST_BC_HH`; do
	# Determine forecast hour FH (2 digits) and forecast day of EPS files
	FH=`expr $hh % 24`	|| true
	FD=$((hh/24))		|| true
	if [ ${FH} -lt 10 ] ; then  FH=0$FH ; fi

	# Loop over members EM and add file names to file_list
	for EM in `seq 1 $TOT_ENS_MEMB`; do 
            if [ ${EM} -lt 10 ] ; then  EM=0$EM ; fi
	    file_list=("${file_list[@]}" "lfff0${FD}${FH}0000_${EM}_$DATE${TIME:0:2}.grb.gz")
	done
    done
}

get_cleanup() {
    putarki_configured_end $PROCNAME
}

get_one() {
    # Download files specified in "file_list" array. If a file is downloaded,
    # it is removed from the array. The function returns 0 if all files
    # have been downloaded (i.e. "file_list" became an empty array), 
    # otherwise it returns 1.
    while true; do
	delete=()
	for ind in "${!file_list[@]}"; do
	    # Download file 
	    fname="${file_list[ind]}"
	    filepath="${FTPDIR}/${fname}"
	    log "starting download of $filepath"
            ncftpget -V -f $WORKDIR_BASE/nwprun/.auth/meteoam_cineca.cfg . $filepath || continue

	    # If the downloaded file is not empty, it is unzipped and its index
 	    # in "file_list" is saved to be deleted later. Reverse order in 
	    # "delete" array must be kept!
	    if [ -s $fname ]; then 
		gunzip $fname > ${fname%.gz} # keep original file
		log "file $fname successfully downloaded and unpacked"
		putarki_configured_archive $PROCNAME ${fname%.gz}
		rm -f ${fname%.gz}
		log "file ${fname%.gz} successfully sent to archive"
		delete=($ind "${delete[@]}")
	    fi
	done

	# if nothing has been done exit, otherwise make suddendly a
	# new trial in case something appeared in the meantime
    	if [ ${#delete[@]} -eq 0 ]; then
            break
    	fi

	# Remove downloaded files from the "file_list"
	for del in ${delete[@]}; do
 	    file_list=( "${file_list[@]:0:$del}"  "${file_list[@]:$del+1}")
	done
	unset delete
    done

    if [ ${#file_list[@]} -eq 0 ]; then
	return 0
    else
	return 1
    fi
}

# enter main loop
main_loop "$@"
