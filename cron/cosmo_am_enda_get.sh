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

		tmpdir=`mktemp -d $PWD/tmptar.XXXXXXXXXX`
		tar --transform='s?.*/??g' -C $tmpdir -xvf $fname
		for file in $tmpdir/*; do
                    nmemb=${file##*l?ff????0000_}
                    nmemb=${nmemb%%_*.grb}
		    # grib_set -s 'subCentre=98,setLocalDefinition=28,localDefinitionNumber=28,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=1,numberOfForecastsInEnsemble=20,baseDateEPS=20161130,baseTimeEPS=00,numberOfRepresentativeMember=0,numberOfMembersInCluster=20,totalInitialConditions=20' input.raw cleps.out
                    # grib_set -s "subCentre=98,setLocalDefinition=1,localDefinitionNumber=1,marsClass=co,marsType=pf,marsStream=enfo,experimentVersionNumber=0001,perturbationNumber=$nmemb,numberOfForecastsInEnsemble=40" $file $file.ls.grib
		    # grib_set -s "typeOfProcessedData=5,productDefinitionTemplateNumber=1,typeOfGeneratingProcess=4,typeOfEnsembleForecast=192,perturbationNumber=$nmemb,numberOfForecastsInEnsemble=40" $file $file.ls.grib
		    putarki_configured_archive $1 $file
# dirty trick for syncing to galileo
		    if [ "$HPC_SYSTEM" = "meucci" ]; then
			rsync -a $file login09.galileo.cineca.it:/gpfs/meteo/lami/import/configured/$dirname:$DATE$TIME:$ENS_MEMB:.$$ || true
		    fi		    
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
export NWPCONF=prod/COSMO_AM_ENDA

set -x
set -e
# source the main library module
. $NWPCONFBINDIR/nwpconf.sh
# source other optional modules
. $NWPCONFBINDIR/putarki.sh
. $NWPCONFBINDIR/arki_tools.sh
. $NWPCONFBINDIR/nwpwait.sh
# end of setup

nonunique_exit

# improve
ncftpauth="-f $basedir/.auth/meteoam_cineca.cfg"

if [ -n "$1" ]; then # interactive run
    DATETIME=$1
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}
else # automatic run
    # redirect all to logfile
    exec >>$LOGDIR/`basename $0`.log 2>&1

    restore_state cosmo_am_enda_get.state || touch $NWPCONFDIR/$NWPCONF/cosmo_am_enda_get.state

    if [ -z "$DATETIME" ]; then # set minimum datetime
	DATETIME=`date -u --date '1 day ago' '+%Y%m%d12'`
    else # increment datetime
	DATETIME=`datetime_add $DATETIME $COSMO_AM_ENDA_STEP`
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

safe_rm_rf $COSMO_AM_ENDA_WORKDIR
mkdir -p $COSMO_AM_ENDA_WORKDIR
cd $COSMO_AM_ENDA_WORKDIR

dirname=cosmo_am_enda
putarki_configured_setup $dirname "reftime=$DATETIME" "format=grib" "signal=cosmo_am_enda"
# dirty trick for syncing to galileo, improve :$DATE$TIME:$ENS_MEMB:.$$!!!
if [ "$HPC_SYSTEM" = "meucci" ]; then
    rsync -a $ARKI_IMPDIR/configured/$dirname:$DATE$TIME:$ENS_MEMB:.$$ login09.galileo.cineca.it:/gpfs/meteo/lami/import/configured || true
fi
nwpwait_setup

# Create array of files to be downloaded
file_list=(lfff00000000_$DATETIME.tar.bz2 lfff00030000_$DATETIME.tar.bz2 lfff00060000_$DATETIME.tar.bz2)

while true; do
    dl_ftp $dirname && break
    nwpwait_wait || break
done

putarki_configured_end $dirname
# dirty trick for syncing to galileo
if [ "$HPC_SYSTEM" = "meucci" ]; then
    rsync -a $ARKI_IMPDIR/configured/$dirname:$DATE$TIME:$ENS_MEMB:.$$/end.sh login09.galileo.cineca.it:/gpfs/meteo/lami/import/configured/$dirname:$DATE$TIME:$ENS_MEMB:.$$ || true
fi

if [ -n "$1" ]; then # interactive run
    :
else # automatic run
    save_state cosmo_am_enda_get.state DATETIME
fi
