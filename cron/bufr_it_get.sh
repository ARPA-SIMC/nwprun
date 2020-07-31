#!/bin/bash
# script for downloading the new bufr provided by COMET (CNMCA)

log() {
    echo `date -u --rfc-3339=seconds` "|$$|$@"
}

dl_ftp() {

    filelist=""
    for obstype in $OBSTYPES; do
	filelist="$filelist ${FTPDIR}${obstype}_$DATETIME.bz2"
    done

    ncftpget -V $ncftpauth . $filelist || true

# check whether anything has been downloaded
    dlfilelist=""
    shopt -s nullglob
    for file in ????_$DATETIME.bz2; do
	dlfilelist="$dlfilelist $file"
    done
    shopt -u nullglob

# something found, wait and retry (we don't know the file-creation policy)
# please optimize
    if [ -n "$dlfilelist" ]; then
	sleep 180
	ncftpget -V $ncftpauth . $filelist || true
	shopt -s nullglob
	for file in ????_$DATETIME.bz2; do
	    bunzip2 -c $file > ${file%.bz2}.bufr
	    putarki_configured_archive $1 ${file%.bz2}.bufr
	done
	return 0
    fi
    return 1

}

unset LANG
basedir=$OPE
# setup common to user scripts
# basic variables
export NWPCONFDIR=$basedir/conf
export NWPCONFBINDIR=$basedir/libexec/nwpconf
export NWPCONF=prod/BUFR_IT

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
ncftpauth="-f $basedir/.auth/bufr_ruc_get.cfg"

if [ -n "$1" ]; then # interactive run
    DATETIME=$1
    DATE=${DATETIME:0:8}
    TIME=${DATETIME:8:2}
else # automatic run
    # redirect all to logfile
    exec >>$LOGDIR/`basename $0`.log 2>&1

    restore_state bufr_it_get.state || touch $NWPCONFDIR/$NWPCONF/bufr_it_get.state
    if [ -z "$DATETIME" ]; then # set minimum datetime
	DATETIME=`date -u --date '1 day ago' '+%Y%m%d12'`
    else # increment datetime
	DATETIME=`datetime_add $DATETIME $BUFR_IT_STEP`
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

safe_rm_rf $BUFR_IT_WORKDIR
mkdir -p $BUFR_IT_WORKDIR
cd $BUFR_IT_WORKDIR

dirname=bufr_it
# change signal name to bufr_it
putarki_configured_setup $dirname "reftime=$DATETIME" "format=bufr" "signal=cnmc_bufr"
nwpwait_setup

while true; do
    dl_ftp $dirname && break
    nwpwait_wait || break
done

putarki_configured_end $dirname

if [ -n "$1" ]; then # interactive run
    :
else # automatic run
    save_state bufr_it_get.state DATETIME
fi
