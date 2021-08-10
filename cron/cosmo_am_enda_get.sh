#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=cosmo_am_enda_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=cosmo_am_enda"
# Create array of files to be downloaded
    file_list=(lfff00000000_$DATE${TIME:0:2}.tar.bz2 lfff00030000_$DATE${TIME:0:2}.tar.bz2 lfff00060000_$DATE${TIME:0:2}.tar.bz2)
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
            ncftpget -V -f $WORKDIR_BASE/nwprun/.auth/meteoam_cineca.cfg . $filepath  || continue

	    # If the downloaded file is not empty, it is unzipped and its index
 	    # in "file_list" is saved to be deleted later. Reverse order in 
	    # "delete" array must be kept!
	    if [ -s $fname ]; then 
		tmpdir=`mktemp -d $PWD/tmptar.XXXXXXXXXX`
		tar --transform='s?.*/??g' -C $tmpdir -xvf $fname
		log "file $fname successfully downloaded and unpacked"
		for file in $tmpdir/*; do
                    nmemb=${file##*l?ff????0000_}
                    nmemb=${nmemb%%_*.grb}
		    putarki_configured_archive $PROCNAME $file
		    log "file $file successfully sent to archive"
		    rm -f $file
		done
		safe_rm_rf $tmpdir
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
