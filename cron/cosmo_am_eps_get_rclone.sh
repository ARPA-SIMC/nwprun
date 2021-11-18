#!/bin/bash

# source common get_ procedures
. `dirname $0`/get_common.sh

# define custom functions
get_init() {
    export PROCNAME=cosmo_am_eps_get
}

get_setup() {
    putarki_configured_setup $PROCNAME "reftime=$DATE$TIME" "format=grib" "signal=cosmo_am_eps"
    file_pattern="lfff0???0000_??_$DATE${TIME:0:2}.grb.gz"
    file_processed=()
    file_list_file=${PROCNAME}_list.$$
    rm -f $file_list_file
    for hh in `seq $FIRST_BC_HH $FREQ_BC_HH $LAST_BC_HH`; do
	# Determine forecast hour FH (2 digits) and forecast day of EPS files
	FH=`expr $hh % 24`	|| true
	FD=$((hh/24))		|| true
	if [ ${FH} -lt 10 ] ; then  FH=0$FH ; fi

	# Loop over members EM and add file names to file_list
	for EM in `seq 1 $TOT_ENS_MEMB`; do
            if [ ${EM} -lt 10 ] ; then  EM=0$EM ; fi
	    echo "lfff0${FD}${FH}0000_${EM}_$DATE${TIME:0:2}.grb.gz"
	done
    done > $file_list_file
    file_count=`cat $file_list_file|wc -l `

}

get_cleanup() {
    putarki_configured_end $PROCNAME
    rm -f $file_list_file
}

get_one() {
    while true; do
	donenothing=Y
	rclone --transfers 8 --include-from $file_list_file copy meteoam-cin:$FTPDIR/ .
	for file in $file_pattern; do
	    alreadydone=
	    if [ -f "$file" ]; then
		for file_test in ${file_processed[@]}; do
		    if [ "$file" = "$file_test" ]; then
			alreadydone=Y
		    fi
		done
		if [ -z "$alreadydone" ]; then
		    # process $file
		    gunzip -c $file > ${file%.gz} # keep original file
		    log "file $file successfully downloaded and unpacked"
		    putarki_configured_archive $PROCNAME ${file%.gz}
		    log "file ${file%.gz} successfully sent to archive"
		    rm -f ${file%.gz}
		    file_processed[${#file_processed[@]}]=$file
		    donenothing=
		fi
	    fi
	done
	if [ "${#file_processed[@]}" -ge "$file_count" ]; then
	    return 0
	fi
	if [ -n "$donenothing" ]; then
	    return 1
	fi
    done
}

# enter main loop
main_loop "$@"
