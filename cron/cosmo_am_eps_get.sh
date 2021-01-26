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
            ncftpget -V $ncftpauth . $filepath  || continue

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

final_cleanup() {
    trap - EXIT
    exit
}

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/COSMO_AM_EPS

set -x
set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/arki_tools.sh
. $NWPCONFBINDIR/nwpwait.sh
# end of setup


# improve
ncftpauth="-f $basedir/.auth/meteoam_cineca.cfg"

if [ -n "$1" ]; then # interactive run
    COSMO_AM_EPS_WORKDIR=${COSMO_AM_EPS_WORKDIR}_interactive
    DATETIME=$1
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}
else # automatic run
    nonunique_exit
    # redirect all to logfile
    exec >>$LOGDIR/`basename $0`.log 2>&1

    restore_state cosmo_am_eps_get.state || touch $NWPCONFDIR/$NWPCONF/cosmo_am_eps_get.state

    if [ -z "$DATETIME" ]; then # set minimum datetime
	DATETIME=`date -u --date '1 day ago' '+%Y%m%d12'`
    else # increment datetime
	DATETIME=`datetime_add $DATETIME $COSMO_AM_EPS_STEP`
    fi
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}

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
fi

safe_rm_rf $COSMO_AM_EPS_WORKDIR
mkdir -p $COSMO_AM_EPS_WORKDIR
cd $COSMO_AM_EPS_WORKDIR

dirname=cosmo_am_eps_fc
putarki_configured_setup $dirname "reftime=$DATETIME" "format=grib" "signal=cosmo_am_eps"
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
	file_list=("${file_list[@]}" "lfff0${FD}${FH}0000_${EM}_$DATETIME.grb.gz")
    done
done 

while true; do
    dl_ftp $dirname && break
    nwpwait_wait || break
done

putarki_configured_end $dirname

if [ -n "$1" ]; then # interactive run
    :
else # automatic run
    save_state cosmo_am_eps_get.state DATETIME
fi
