#!/bin/bash
# script for downloading cnmc eps boundary data

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

dl_ftp() {
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
            ncftpget $ncftpauth . $filepath  || continue

	    # If the downloaded file is not empty, it is unzipped and its index
 	    # in "file_list" is saved to be deleted later. Reverse order in 
	    # "delete" array must be kept!
	    if [ -s $fname ]; then 
		gunzip $fname
		putarki_configured_archive $1 ${fname%.gz}
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

increment_datetime() {
    DATE=`date --date="$DATE $TIME $COSMO_AM_EPS_STEP seconds" '+%Y%m%d'`
    TIME=`date --date="$DATE $TIME $COSMO_AM_EPS_STEP seconds" '+%H%M'`
}

final_cleanup() {
    trap - EXIT
    exit
}

set -x
unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/COSMO_AM_EPS

set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/arki_tools.sh
. $NWPCONFBINDIR/nwpwait.sh
# end of setup

nonunique_exit

# redirect all to logfile
exec >>$LOGDIR/`basename $0`.log 2>&1

set -x

safe_rm_rf $COSMO_AM_EPS_WORKDIR
mkdir -p $COSMO_AM_EPS_WORKDIR
cd $COSMO_AM_EPS_WORKDIR

# improve
ncftpauth="-f $basedir/.auth/meteoam_cineca.cfg"

if [ -n "$1" ]; then
    dl_ftp $1 $2
else
    restore_state cosmo_am_eps_get.state || touch $NWPCONFDIR/$NWPCONF/cosmo_am_eps_get.state
    if [ -z "$DATE" -o -z "$TIME" ]; then 	# set minimum datetime
	mindate=`date -u --date '1 day ago' '+%Y%m%d 12'`
	DATE=`date --date="$mindate" '+%Y%m%d'`
	TIME=`date --date="$mindate" '+%H'`
    else 	# increment datetime
	increment_datetime
    fi
# wait before querying the server
    NWPWAITSOLAR_SAVE=$NWPWAITSOLAR
    NWPWAITSOLAR=$NWPWAITSOLAR_RUN
    NWPWAITWAIT_SAVE=$NWPWAITWAIT
    unset NWPWAITWAIT
    nwpwait_setup
    nwpwait_wait && exit 0 # too early, try next time

# wait until reasonable
    NWPWAITSOLAR=$NWPWAITSOLAR_SAVE
    NWPWAITWAIT=$NWPWAITWAIT_SAVE

    dirname=cosmo_am_eps_fc_$DATE$TIME.$$
    putarki_configured_setup $dirname "reftime=$DATE$TIME" "format=grib" "signal=cosmo_am_eps_fcast"
    nwpwait_setup

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

# trovare una maniera per non cominciare dal wait nel loop
    while nwpwait_wait; do
	dl_ftp $dirname && break
    done

    putarki_configured_end $dirname

    save_state cosmo_am_eps_get.state DATE TIME

fi
